const toggle = cls => elem => elem.classList.toggle(cls);
const toggleActive = toggle('is-active');
const toggleLoading = toggle('is-loading');

for (const navbarBurger of document.querySelectorAll('.navbar-burger')) {
  const target = document.getElementById(navbarBurger.dataset.target);

  navbarBurger.addEventListener('click', () => {
    toggleActive(navbarBurger);
    toggleActive(target);
  });
}