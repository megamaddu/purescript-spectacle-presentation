var Main = require('../src/Main.purs');
var debug = process.env.NODE_ENV === 'development'

if (module.hot) {
	var app = Main[debug ? 'debug' : 'main'](window.puxLastState || {})();
	app.state.subscribe(function (state) {
	 window.puxLastState = state;
	});
	module.hot.accept();
} else {
	Main[debug ? 'debug' : 'main']({})();
}
