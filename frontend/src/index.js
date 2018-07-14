import { Scheduler } from './Scheduler.elm';
import { View } from './View.elm';
import { Done } from './Done.elm';
import { History } from './History.elm';
import { Login } from './Login.elm';
import { Alter } from './Alter.elm';
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
let token = localStorage.getItem('Authorization')
let flags = {
	apiEndpoint: process.env.ELM_APP_API_URL,
	authToken: token
}
let obj = null
let parentName = null

switch (page) {
	case "log":
		obj = History
		break
	case "scheduler":
		obj = Scheduler
		break
	case "add":
	case "alter":
		obj = Alter
		flags.subjectName = getParameterByName("subjectName") || ""
		flags.parentName = getParameterByName("parent") || ""
		break
	case "view":
		obj = View
		flags.subjectName = getParameterByName("subjectName") || ""
		break
	case "done":
		obj = Done
		flags.subjectName = getParameterByName("subjectName") || ""
		break
	default:
		page="login"
		obj = Login
}

if (!token && page != "login") {
	window.location.href = "/";
}


console.log(process.env.ELM_APP_API_URL)
obj.embed(document.getElementById('root'), flags)


registerServiceWorker();
