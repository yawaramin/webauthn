module Simple = Webauthn.Simple

let or_failwith msg = function Some v -> v | None -> failwith msg
let or_invalid_arg msg = function Some v -> v | None -> invalid_arg msg

let or_invalid = function
  | Ok v -> v
  | Error e -> invalid_arg (Fmt.str "%a" Webauthn.pp_error e)

let or_not_found = function Some v -> v | None -> raise Not_found

(* Simplified passkey storage. Of course, in production these would be in some
   persistent storage. *)

type user_credentials = {
  user_id : string;
  user_name : string;
  credential_ids : string list;
}

(* user_id, user_credentials *)
let users_credentials = Hashtbl.create 8

(* credential_id, passkey *)
let credential_passkeys = Hashtbl.create 8

let add_user ~user_name user_id =
  if not (Hashtbl.mem users_credentials user_id) then
    Hashtbl.replace users_credentials user_id {
      user_id;
      user_name;
      credential_ids = [];
    }

let lookup_user user_id = user_id
  |> Hashtbl.find_opt users_credentials
  |> or_failwith (Fmt.str "lookup_user: User %s not found" user_id)

let add_passkey ({ Simple.user_id; credential_id; _ } as passkey) =
  let user_credentials = lookup_user user_id in
  let user_credentials = {
    user_credentials with
    credential_ids = credential_id :: user_credentials.credential_ids;
  }
  in
  Hashtbl.replace credential_passkeys credential_id passkey;
  Hashtbl.replace users_credentials user_id user_credentials

let lookup_passkey credential_id = credential_id
  |> Hashtbl.find_opt credential_passkeys
  |> or_failwith (Fmt.str "lookup_passkey: Passkey %s not found" credential_id)

let lookup_passkeys user_id = List.map lookup_passkey (lookup_user user_id).credential_ids
let put_flash req = Dream.add_flash_message req "info"
let get_flash req = req |> Dream.flash_messages |> List.assoc_opt "info"

let session_field_name = "user.id"
let challenge_field_name = "webauthn.challenge"
let user_id_field_name = "webauthn.user_id"
let no_store = "Cache-Control", "no-store"

let home = Dream_html.get Path.home (fun req ->
  let user_id = Dream.session_field req session_field_name in
  let user_name = user_id |> Option.map (fun uid -> (lookup_user uid).user_name) in
  let passkeys = match user_id with
    | Some user_id -> lookup_passkeys user_id
    | None -> []
  in
  Dream_html.respond (View.home ?flash:(get_flash req) ?user_name ~passkeys ()))

let signup = Dream_html.get Path.signup (fun _ -> Dream_html.respond View.signup)

let register_start webauthn = Dream_html.get Path.register (fun req ->
  let user_name = "user-name" |> Dream.query req |> or_invalid_arg "user-name" in
  let options =
    Simple.generate_registration_options
      ~user_id:user_name
      ~user_name
      ~display_name:user_name
      webauthn
  in
  let%lwt () = Dream.invalidate_session req in
  let%lwt () = Dream.set_session_field req user_id_field_name options.user.id in
  let%lwt () = options.challenge
    |> Webauthn.challenge_to_string
    |> Dream.set_session_field req challenge_field_name
  in
  add_user ~user_name options.user.id;

  options
  |> Simple.public_key_credential_creation_options_to_yojson
  |> Yojson.Safe.to_string
  |> Dream.json ~headers:[no_store])

let register_finish webauthn = Dream_html.post Path.register (fun req ->
  let expected_challenge = challenge_field_name
    |> Dream.session_field req
    |> or_invalid_arg "Missing challenge"
    |> Webauthn.challenge_of_string
    |> or_invalid_arg "Invalid challenge"
  and user_id = user_id_field_name |> Dream.session_field req |> or_not_found in
  let%lwt response = Dream.body req in
  webauthn
  |> Simple.verify_registration_response
    ~expected_challenge
    ~user_id
    ~created_at:(Unix.time ())
    response
  |> or_invalid
  |> add_passkey;

  let%lwt () = Dream.invalidate_session req in
  let%lwt () = Dream.set_session_field req session_field_name user_id in
  put_flash req "Successfully registered!";

  Dream.empty `Created)

let login_start webauthn = Dream_html.get Path.login (fun req ->
  let options = Simple.generate_authentication_options webauthn in
  let%lwt () = Dream.invalidate_session req in
  let%lwt () = options.challenge
    |> Webauthn.challenge_to_string
    |> Dream.set_session_field req challenge_field_name
  in
  options
  |> Simple.public_key_credential_request_options_to_yojson
  |> Yojson.Safe.to_string
  |> Dream.json ~headers:[no_store])

let login_finish webauthn = Dream_html.post Path.login_finish (fun req credential_id ->
  let expected_challenge = challenge_field_name
    |> Dream.session_field req
    |> or_invalid_arg "Missing challenge"
    |> Webauthn.challenge_of_string
    |> or_invalid_arg "Invalid challenge"
  in
  let%lwt response = Dream.body req in
  let passkey = lookup_passkey credential_id in
  let user_credentials = lookup_user passkey.user_id in
  let auth = webauthn
    |> Simple.verify_authentication_response ~expected_challenge ~passkey response
    |> or_invalid
  in
  Hashtbl.replace credential_passkeys credential_id {
    passkey with sign_count = auth.sign_count;
    last_used = Unix.time ();
  };
  put_flash req "Successfully logged in!";

  let%lwt () = Dream.invalidate_session req in
  let%lwt () = Dream.set_session_field req session_field_name user_credentials.user_id in
  Dream.empty `OK)

let logout = Dream_html.post Path.logout (fun req ->
  let%lwt () = Dream.invalidate_session req in
  Dream_html.(redirect req (path_attr HTML.href Path.home)))

let () =
  let webauthn = Result.get_ok (Webauthn.create ~name:"OCaml WebAuthn Demo" "http://localhost:8080") in
  Dream.run
  @@ Dream.logger
  @@ Dream.cookie_sessions
  @@ Dream.flash
  @@ Dream.router [
    home;
    signup;
    register_start webauthn;
    register_finish webauthn;
    login_start webauthn;
    login_finish webauthn;
    logout;
    Static.routes;
  ]
