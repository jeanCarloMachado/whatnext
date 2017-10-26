#!/usr/bin/env node


const args = process.argv;
// console.log(args)

var timeStart = new Date(args[2]).getTime();
var timeEnd = new Date(args[3]).getTime();
var diffInMs = timeEnd - timeStart; //in ms

var humanReadable = {};

var diffInDays = diffInMs / (1000 * 60 * 60 * 24);
if (Math.floor(diffInDays) > 0) {
    humanReadable.days = Math.round(diffInDays);

   diffInMs = diffInMs % (1000 * 60 * 60 * 24);
}

var diffInHours= diffInMs / (1000 * 60 * 60 );
if (Math.floor(diffInHours) > 0) {
    humanReadable.hours = Math.round(diffInHours);
   diffInMs = diffInMs % (1000 * 60 * 60);
}


var diffInMinutes = diffInMs / (1000 * 60);
if (Math.floor(diffInHours) > 0) {
    humanReadable.minutes = Math.round(diffInMinutes);
}

console.log(JSON.stringify(humanReadable)); //{hours: 0, minutes: 30}
