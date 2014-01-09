# corman

Tools to reconfigure Erlang applications and supervisors.

## corman.erl

COnfiguration Reload MAnager. Reload node configuration and call applications callbacks.
This is a part of what Erlang/OTP does when rolling out a new release.
This can be helpful if you do not use Erlang releases or, for some reason, need to re-read a node configuration file (usually sys.config). `corman:reload()` will do the job.

## superman.erl

SUPErvisor Reload MANager. Start/stop/restart supervisor children according to new specifications.
Usage:

    superman:reconfigure_supervisor(SupRef, NewSpecs).

For more usage patterns, see superman.erl.
