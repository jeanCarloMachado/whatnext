#!/usr/bin/env python

import os, sys
import subprocess
import hashlib

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from scheduler import configure_subjects, sort_subjects

from flask import Flask, request, jsonify, Response
from flask_cors import CORS
import json
from detail import get_subject
from gateway import gatewaySuccess,gateway

app = Flask(__name__)
app.debug=True
CORS(app)

SUCCESS_MESSAGE = '{"message": "success"}'


def update_environemnt(my_env, email):
    my_env['WHATNEXT_CONF'] = "/data/whatnext/users/"+email+"/whatnext.conf"
    my_env['WHATNEXT_GOALS'] = "/data/whatnext/users/"+email+"/whatnext_goals.conf"
    my_env['WHATNEXT_HISTORY'] = "/data/whatnext/users/"+email+"/whatnext_history.conf"

    return my_env


def check_authorization(request):
    if not 'Authorization' in request.headers:
        raise '{"message": "Token Required"}'

    loginHash = request.headers['Authorization']
    email = gateway(['getEmailByHash', loginHash])
    if email == '':
        raise '{"message": "Invalid token"}'

    return email

@app.route('/scheduler')
def index():
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)

    tiredMode = request.args.get('tiredMode', default = False, type = bool)
    if tiredMode:
        my_env["TIRED"] = "1"


    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../scheduler.py',
    ]

    my_env["TO_JSON"] = "1"
    content = subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE).stdout.decode('UTF-8')


    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/log')
def log():
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)

    cmd = [ os.path.dirname(os.path.realpath(__file__)) + '/../log.sh', '--json']
    content =  subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE).stdout.decode('UTF-8')

    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/done/<subjectName>', methods = ['POST'])
def done(subjectName):
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)

    data=request.json
    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../done.sh',
        subjectName,
        data['description'],
        data['whatToDoNext']
    ]

    my_env = os.environ.copy()
    my_env["NO_ITERACTIVE"] = "1"
    subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE)

    return SUCCESS_MESSAGE, 200, {'Content-Type': 'application/json; charset=utf-8'}


@app.route('/add', methods = ['POST'])
def add():
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)

    data=request.json
    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../add.sh',
        data['name'],
        str(data['priority']),
        str(data['complexity'])
    ]
    subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE)

    return SUCCESS_MESSAGE, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/rm/<subjectName>', methods = ['GET'])
def rm(subjectName):
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)

    data=request.json
    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../rm.sh',
        subjectName
    ]
    subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE)

    return SUCCESS_MESSAGE, 200, {'Content-Type': 'application/json; charset=utf-8'}


@app.route('/detail/<subjectName>')
def detail(subjectName):
    email = check_authorization(request)
    my_env = update_environemnt(os.environ.copy(), email)


    my_env["TO_JSON"] = "1"

    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../detail.py',
        subjectName
    ]
    result = subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE).stdout.decode('UTF-8')
    return result, 200, {'Content-Type': 'application/json; charset=utf-8'}


@app.route('/signup', methods = ['POST'])
def signup():
    data=request.json

    m = hashlib.sha256()
    m.update(data['email'].encode('utf-8'))
    m.update(data['password'].encode('utf-8'))
    loginHash = m.hexdigest()


    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/signup.sh',
        data['email'],
        loginHash
    ]

    result = subprocess.run(cmd, stdout=subprocess.PIPE)

    if result.returncode != 0:
        return '{"status": "'+str(result.returncode)+'" "message": "' + result.stdout.decode('UTF-8') + '"}', 200, {'Content-Type': 'application/json; charset=utf-8'}


    my_env = os.environ.copy()
    my_env = update_environemnt(my_env, data['email'])

    cmd = [
        os.path.dirname(os.path.realpath(__file__)) + '/../init.sh',
    ]

    result = subprocess.run(cmd, env=my_env, stdout=subprocess.PIPE)
    print(result)



    return SUCCESS_MESSAGE, 200, {'Content-Type': 'application/json; charset=utf-8'}


@app.route('/login', methods = ['POST'])
def login():
    data=request.json


    m = hashlib.sha256()
    m.update(data['email'].encode('utf-8'))
    m.update(data['password'].encode('utf-8'))
    loginHash = m.hexdigest()
    if not gatewaySuccess(['validLogin', data['email'], loginHash]):
        return '{"message": "Invalid user or password"}', 200, {'Content-Type': 'application/json; charset=utf-8'}

    return '{"hash": "'+loginHash+'"}', 200, {'Content-Type': 'application/json; charset=utf-8'}


app.run(host= '0.0.0.0', port=5000)
