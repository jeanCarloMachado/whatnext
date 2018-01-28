import os
import subprocess

def memoize(function):
    from functools import wraps
    memo = {}
    @wraps(function)
    def wrapper(*args):
        if str(args) in memo:
            return memo[str(args)]
        else:
            rv = function(*args)
            memo[str(args)] = rv
            return rv
    return wrapper

def gateway(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).stdout.decode('UTF-8')


def gatewaySuccess(params):
    prefix = [ os.path.dirname(os.path.realpath(__file__)) + '/gateway.sh']
    return subprocess.run(prefix + params, stdout=subprocess.PIPE).returncode == 0
