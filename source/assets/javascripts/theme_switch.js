var ThemeSwitch = (function() {
  var _key   = 'theme',
      _dark  = 'dark',
      _light = 'light';

  function init(target) {
    if (!localStorageAvailable()) {
      return;
    }

    setTheme(currentTheme());

    var checkbox = document.createElement('input');
    checkbox.setAttribute('type', 'checkbox');
    checkbox.checked = currentTheme() == _dark;
    checkbox.addEventListener('change', function(e) {
      this.checked ? setTheme(_dark) : setTheme(_light);
    });

    target.appendChild(checkbox);
  }

  function setTheme(theme) {
    document.body.setAttribute('data-theme', theme);
    localStorage.setItem(_key, theme);
  }

  function currentTheme() {
    var theme = localStorage.getItem(_key) || getDefaultTheme();
    return theme;
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
      var storage = localStorage,
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
