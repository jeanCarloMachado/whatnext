import os
import subprocess

def gateway(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')


def gatewaySuccess(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).returncode == 0
