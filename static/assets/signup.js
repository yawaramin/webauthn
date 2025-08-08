document.getElementById('signup-form')?.addEventListener('submit', async evt => {
  evt.preventDefault();

  const submitBtn = evt.target.querySelector('button[type=submit]');
  toggleLoading(submitBtn);

  const optionsStr = await fetch('/register?user-name=' + document.getElementById('user-name').value);
  const optionsJson = await optionsStr.json();
  const options = PublicKeyCredential.parseCreationOptionsFromJSON(optionsJson);
  const credential = await navigator.credentials.create({ publicKey: options });

  if (credential == null) {
    alert('Could not create passkey');
    return;
  }

  const responseStr = JSON.stringify(credential.toJSON().response);

  await fetch('/register', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: responseStr,
  });

  window.location.href = '/';
});
