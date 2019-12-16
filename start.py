#! /usr/bin/env python3
import subprocess
import pexpect
import sys

child = pexpect.spawn("make shell n=1 PEER_PORT=27001")
child.sendline("lasp_peer_service:join('{}'".format('achlys2@130.104.167.31'))
child.expect("TEST OUTPUT")
print(child.before)