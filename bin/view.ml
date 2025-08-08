open Dream_html

let data_target = string_attr "data-target"
let use = void_tag "use"

let svg_icon icon_path =
  let open HTML in
  let open SVG in
  span [class_ "icon"] [
    svg [height "24px"] [use [
      path_attr href icon_path;
      style_ "fill: hsl(var(--bulma-button-h),var(--bulma-button-s),var(--bulma-button-color-l))"]]]

let page ?(status_box=HTML.null []) ~main_view () =
  let open HTML in
  html [lang "en"] [
    head [] [
      title [] "OCaml WebAuthn Demo";
      meta [charset "utf-8"];
      meta [name "viewport"; content "width=device-width, initial-scale=1"];
      link [rel "stylesheet"; href "https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"]];
    body [] [
      nav [class_ "navbar"; role `navigation; Aria.label "main navigation"] [
        div [class_ "navbar-brand"] [
          h1 [class_ "navbar-item title"] [
            a [path_attr href Path.home] [txt "OCaml WebAuthn Demo"]];
          a [role `button; class_ "navbar-burger"; Aria.label "menu"; Aria.expanded false; data_target "navbar-menu"] [
            span [Aria.hidden true] [];
            span [Aria.hidden true] [];
            span [Aria.hidden true] [];
            span [Aria.hidden true] []]];
        div [class_ "navbar-menu"; id "navbar-menu"] [
          div [class_ "navbar-end"] [status_box]]];
      main [class_ "section"] [main_view];
      script [path_attr src Static.Assets.app_js] ""]]

let status_box ?flash ~user_name () =
  let open HTML in
  let flash_notif = match flash with
    | Some msg ->
        button [
          class_ "button is-static has-background-success-light has-text-success-invert";
          role `alert;
        ] [txt "%s" msg]
    | None -> null []
  in
  match user_name with
  | None ->
      div [class_ "navbar-item"] [
        div [class_ "buttons"] [
          flash_notif;
          a [class_ "button is-primary"; path_attr href Path.signup] [txt "Sign up"];
          button [class_ "button"; id "login-button"] [
            svg_icon Static.Assets.passkey_svg;
            span [] [txt "Log in"]];
          script [path_attr src Static.Assets.login_js] ""]]
  | Some user ->
    null [
      div [class_ "navbar-item"] [
        div [class_ "buttons"] [
          flash_notif;
          button [class_ "button is-static"] [
            svg_icon Static.Assets.account_svg;
            span [] [txt "%s" user]];
          button [class_ "button"; type_ "submit"; form_ "logout-form"] [txt "Log out"]]];
      form [path_attr action Path.logout; method_ `POST; id "logout-form"] []]

let date_format f =
  let {
    Unix.tm_year;
    tm_mon;
    tm_mday;
    tm_hour;
    tm_min;
    tm_sec;
    _
  } = Unix.localtime f
  in
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (tm_year + 1900)
    (tm_mon + 1)
    tm_mday
    tm_hour
    tm_min
    tm_sec

let passkey_card { Webauthn.Simple.created_at; last_used; _ } =
  let open HTML in
  div [class_ "cell"] [
    div [class_ "card"] [
      header [class_ "card-header"] [
        p [class_ "card-header-title"] [txt "Passkey"];
        button [class_ "card-header-icon is-static"; Aria.label "key"] [
          svg_icon Static.Assets.key_svg]];
      div [class_ "card-content"] [
        div [class_ "content"] [
          p [] [txt "Created: %s" (date_format created_at)];
          p [] [txt "Last used: %s" (date_format last_used)]]]]]

let passkey_list passkeys =
  let open HTML in
  hgroup [] [
    h2 [class_ "title is-4"] [txt "Passkeys"];
    div [class_ "grid"] (List.map passkey_card passkeys)]

let home ?flash ?user_name ~passkeys () =
  page ~status_box:(status_box ?flash ~user_name ()) ~main_view:(passkey_list passkeys) ()

let signup =
  let open HTML in
  page ~status_box:(status_box ~user_name:None ()) ~main_view:(
    null [
      form [id "signup-form"] [
        div [class_ "field"] [
          label [for_ "user-name"; class_ "label"] [txt "Username"];
          div [class_ "control"] [
            input [
              class_ "input";
              name "user-name";
              id "user-name";
              autocomplete `username;
              autofocus]]];
        div [class_ "field"] [
          div [class_ "control"] [
            button [class_ "button is-primary"; type_ "submit"] [txt "Sign up"]]]];
      script [path_attr src Static.Assets.signup_js] ""]) ()
