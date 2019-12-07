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
    var theme = localStorage.getItem(_key) || 'light';
    return theme;
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
