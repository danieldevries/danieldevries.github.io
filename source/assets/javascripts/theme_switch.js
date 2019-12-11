var ThemeSwitch = (function() {
  var _switcher = null,
      _key = 'theme';

  function init(switcher) {
    if (!localStorageAvailable()) {
      return;
    }

    setTheme(currentTheme());

    this._switcher = switcher;
    this._switcher.onclick = function(e) {
      e.preventDefault();
      toggleTheme(currentTheme());
    }
  }

  function toggleTheme(value) {
    var newTheme = { 'dark': 'light', 'light': 'dark' }[value]
    setTheme(newTheme);
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
      return 'dark';
    }
    if (window.matchMedia('(prefers-color-scheme: light)').matches ||
        window.matchMedia('(prefers-color-scheme: no-preference)').matches) {
      return 'light';
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
      console.log(e);
      return false;
    }
  }

  return {
    init: init
  };
})();
