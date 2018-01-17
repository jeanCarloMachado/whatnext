all:
	rm -rf /tmp/whatnext || true
	git clone ssh://aur@aur.archlinux.org/whatnext.git /tmp/whatnext
	makepkg --printsrcinfo > /tmp/whatnext/.SRCINFO
	cp PKGBUILD  /tmp/whatnext
	cd /tmp/whatnext ; git add .
	cd /tmp/whatnext ; git commit -m 'wip'
	cd /tmp/whatnext ; git push

build:
	makepkg -rsi -f

md5sha:
	makepkg -g

