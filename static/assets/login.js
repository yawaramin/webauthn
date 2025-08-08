document.getElementById('login-button')?.addEventListener('click', async evt => {
  toggleLoading(evt.target);

  const optionsStr = await fetch('/login');
  const optionsJson = await optionsStr.json();
  const options = PublicKeyCredential.parseRequestOptionsFromJSON(optionsJson);
  const credential = await navigator.credentials.get({ publicKey: options });

  if (credential == null) {
    alert('Could not retrieve passkey');
    return;
  }

  await fetch('/login', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({
      response: JSON.stringify(credential.toJSON().response),
      'credential-id': credential.id,
    }),
  });

  window.location.href = '/';
});
