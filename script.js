let timerSpan = document.getElementById("timer");

let timers = {
    lastStarWarsReference: 0
}

setLastStarWarsReference(() => {
    setInterval(() => {
        let elapsed = (Date.now() - timers.lastStarWarsReference) / 1000;
        let elapsedDays = Math.floor(elapsed / 86400);
        let elapsedHours = Math.floor((elapsed % 86400) / 3600);
        let elapsedMinutes = Math.floor((elapsed % 3600) / 60);
        let elapsedSeconds = (elapsed % 60).toFixed(3);
        timerSpan.innerHTML = `${elapsedDays} days, ${elapsedHours} hours, ${elapsedMinutes} minutes, ${elapsedSeconds} seconds.`;
    }, 200 * Math.random());
});

function setLastStarWarsReference (callback) {
    let request = new XMLHttpRequest();
    request.open("GET", "https://json.extendsclass.com/bin/e3f8eb195b58", true);
    request.onreadystatechange = e => {
        if (request.readyState === 4) {
            timers = JSON.parse(request.responseText);
            callback();
        }
    };
    request.send();
}

function resetLastStarWarsReference () {
    let request = new XMLHttpRequest();
    request.open("PUT", "https://json.extendsclass.com/bin/e3f8eb195b58", true);
    //request.setRequestHeader("Security-key", "Your security key");
    timers.lastStarWarsReference = Date.now();
    request.send(JSON.stringify(timers));
}

document.getElementById("reset").addEventListener("click", resetLastStarWarsReference);
