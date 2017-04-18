#!/usr/bin/env /usr/local/bin/node

var CircleCI = require('circleci');

var ci = new CircleCI({
  auth: "PUT THE TOKEN HERE!!!"
});



function createMessageCommit(message, build) {
	message += build.reponame;
	message += ' #' + build.build_num;
	message += ' ' + build.subject;
	message += ' - ' + build.author_name;

	message += ' | href=' + build.build_url

	return message;
}

function createMessageDeploy(message, build) {
	message += build.reponame;
	message += ' #' + build.build_num;
	message += ' ' + build.vcs_tag;

	message += ' | href=' + build.build_url;

	return message;
}

function applyEmoji(message, build) {
	if (build.status == 'success') {
		message += '👌 ';
	} else if (build.status == 'running') {
		message += '🏃 ';
	} else if (build.status == 'scheduled') {
		message += '⏳ ';
	} else {
		message += '⛔ ';
	}

	return message;
}

function createMessage(build) {
	var msg = '';
	
	msg = applyEmoji(msg, build);

	if (build.vcs_tag) {
		msg = createMessageDeploy(msg, build);
	} else {
		msg = createMessageCommit(msg, build);
	}

	return msg;
}


ci.getRecentBuilds({ limit: 21 })
  .then(function(builds){
  	console.log(createMessage(builds[0]));

    console.log('---');

    builds = builds.slice(1);

  	for(var i=0; i < builds.length; i++) {
	  console.log(createMessage(builds[i]));
	}
  });


