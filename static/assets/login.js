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

  await fetch('/login/' + credential.id, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(credential.toJSON().response),
  });

  window.location.href = '/';
});
