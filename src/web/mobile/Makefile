currentDir = $(shell pwd)
releaseDir=${currentDir}/platforms/android/app/build/outputs/apk/release

all: browser

installApk:
	adb install ${releaseDir}/android.apk
uninstall:
	adb uninstall net.thewhatnext.app

browser:
	phonegap run browser

clearwww:
	rm -rf www/*

copyDist: clearwww
	cp -rf ../../../dist/* www/

testAndroid: copyDist
	phonegap run android


releaseAndroid: copyDist
	rm ${releaseDir}/*.apk || true
	phonegap build android --release
	cd ${releaseDir} ; jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore /home/jean/Dropbox/whatnext-release-key.keystore app-release-unsigned.apk whatnext
	cd ${releaseDir} ; zipalign -v 4 app-release-unsigned.apk android.apk
	echo "The files are in: ${releaseDir} "



installRelease:
	adb install ./platforms/android/app/build/outputs/apk/release/android.apk
