# Hicks

Hicks happily spawns machines.

Hicks is a simple tool to spawn and provision machines. Currently only
[UpCloud](http://upcloud.com/) is supported but in the future, other services
should be added (e.g. [DigitalOcean](https://www.digitalocean.com/),
[Linode](https://www.linode.com/), [Vagrant](https://www.vagrantup.com/), or
already existing hosts (e.g. physical servers rented by the month)).

## Getting started

Hicks is in development and not yet released on Hackage. To install, clone this
repository and run `cabal install`:

    > git clone https://github.com/noteed/hicks.git
    > cd hicks
    > cabal install

Make sure you have `~/.cabal/bin` in your `PATH` and you can run `hicks --help`
to see the available commands.

You need an account on UpCloud. You also need to generate API credentials for
your account that you paste in `secret/upcloud-key.txt`.

    > cat secret/upcloud-key.txt
    api_username/api_password

At this point you should be able to run:

    > hicks account
    username: noteed
    credits: 4625.72

This command reports your username and available credits (in cents).

## Next steps

Have a look at the `test-against-upcloud.sh` script. It shows the basic
operations you can perform to create a machine on UpCloud, wait for it until it
is up and running, and do some basic provisioning.

When you create a machine, you use

    > hicks create <some-hostname>

Provisioning consists of simply uploading a file hierarchy locally stored in
the `provision/<some-hostname>` director. In particular, it should contain a
script located at `root/bin/provision` that will be run after upload.

## Notes

UpCloud doesn't allow to set a public SSH key automatically when a machine is
created. Instead Hicks will store the root password return by UpCloud in
`~/.hicks` until the `hicks authorize` command is run. That command will upload
a public SSH key you choose. Further disabling the root password should be done
in the provisioning script.

## Limitations

- Only UpCloud is supported.
- Some configuration values are hard-coded (e.g. uk-lon1 or Ubuntu 12.04).
