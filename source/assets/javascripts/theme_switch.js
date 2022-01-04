var ThemeSwitch = (function() {
  var _key   = 'theme',
      _dark  = 'dark',
      _light = 'light';

  function init(target) {
    if (!localStorageAvailable()) {
      return;
    }

    setTheme(currentTheme());

    target.addEventListener('click', () => {
      toggleTheme();
    });
  }

  function toggleTheme() {
    if (currentTheme() == _dark) {
      setTheme(_light);
    } else {
      setTheme(_dark);
    }
  }

  function setTheme(theme) {
    document.body.setAttribute('data-theme', theme);
    localStorage.setItem(_key, theme);
  }

  function currentTheme() {
    return localStorage.getItem(_key) || getDefaultTheme();
  }

  function getDefaultTheme() {
    if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
      return _dark;
    }
    if (window.matchMedia('(prefers-color-scheme: light)').matches ||
        window.matchMedia('(prefers-color-scheme: no-preference)').matches) {
      return _light;
    }
  }

  function localStorageAvailable() {
    try {
      let storage = localStorage,
          x = '__local_storage_test__';
      storage.setItem(x, x);
      storage.removeItem(x);
      return true;
    } catch (e) {
      return false;
    }
  }

  return {
    init: init
  };
})();
