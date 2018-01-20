# Steps to setup Hoogle on haskell.org

The machine `hoogle.haskell.org` is located at `104.130.162.85` and runs the Hoogle instance. These instructions set up the machine for automatic updates 8pm every day.

## Initial setup

### As `root`

Install GHC, git and zlib (a required library).

	add-apt-repository -y ppa:hvr/ghc
	apt-get update
	apt-get install ghc-8.2.2 cabal-install-1.24 happy-1.19.4 alex-3.1.3
	apt-get install git
	apt-get install zlib1g-dev

Create a swap file using the instructions [originally from here](https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04):

	fallocate -l 4G /swapfile
	chmod 600 /swapfile
	mkswap /swapfile
	swapon /swapfile

Remap port 80 to 8080 so non-privileged processes can talk on port 80, using the instructions [originally from here](http://unix.stackexchange.com/questions/10735/linux-allowing-an-user-to-listen-to-a-port-below-1024/10791#10791):

	modprobe ip_tables
	echo 'ip_tables' >> /etc/modules
	iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-ports 8080

Create the user `www` configured for SSH access.

### As `www`

Add the GHC/Cabal binaries to the `$PATH` in the `~/.profile`.

	export PATH=/home/www/.cabal/bin:/opt/ghc/7.8.4/bin:/opt/cabal/1.18/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH

Create a shell script `update.sh`:

	cd /home/www
	wget https://raw.githubusercontent.com/ndmitchell/hoogle/master/misc/Upgrade.hs -O - --no-check-certificate --quiet | runhaskell

Then configure updating every day at 8pm. Add a Cron job by using `crontab -e` and adding the line:

	0 20 * * * bash -l /home/www/update.sh > /home/www/update.txt 2>&1

## Monitoring

* `df -h`, check there is sufficient disk space.
* `top`, see what is running.

Currently monitored with [uptimerobot.com](http://uptimerobot.com/).

## Alternatives and notes

At some point I added the line `/swapfile none swap sw 0 0` to the bottom of `/etc/fstab`. Not sure if that is required or not.

To run on port 80, as root do (as per [here](http://stackoverflow.com/questions/413807/is-there-a-way-for-non-root-processes-to-bind-to-privileged-ports-1024-on-l#414258)):

    setcap 'cap_net_bind_service=+ep' /home/www/.cabal/bin/hoogle

`relrod` on IRC says for port 80 usually we've been doing this (or at least I have for hl): Have the app server run and listen on localhost, then throw nginx front of it as a proxy. Let nginx handle things like SSL and caching (and binding to port 80 and 443). Not really a guide, but you can see the config in the ansible repo..
