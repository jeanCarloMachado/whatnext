import { Scheduler } from './Scheduler.elm';
import { History } from './History.elm';
import { Login } from './Login.elm';
import registerServiceWorker from './registerServiceWorker';

function getParameterByName(name, url) {
	if (!url) url = window.location.href;
	name = name.replace(/[\[\]]/g, "\\$&");
	var regex = new RegExp("[?&]" + name + "(=([^&#]*)|&|#|$)"),
		results = regex.exec(url);
	if (!results) return null;
	if (!results[2]) return '';
	return decodeURIComponent(results[2].replace(/\+/g, " "));
}

let page = getParameterByName("page") || "index"
let obj = null
switch (page) {
	case "log":
		obj = History
		break
	case "scheduler":
		obj = Scheduler
		break
	default:
		page="login"
		obj = Login
}

let token = localStorage.getItem('Authorization')
if (!token && page != "login") {
	window.location.href = "/";
}


console.log(process.env.ELM_APP_API_URL)
obj.embed(document.getElementById('root'), {
	apiEndpoint: process.env.ELM_APP_API_URL,
	authToken: token
})


registerServiceWorker();
