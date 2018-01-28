#!/usr/bin/env python
import os, sys
import subprocess

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from scheduler import configure_subjects, sort_subjects

from flask import Flask, request, jsonify, Response
from flask_cors import CORS
import json
from detail import get_subject

app = Flask(__name__)
app.debug=True
CORS(app)

@app.route('/scheduler')
def index():
    tiredMode = request.args.get('tiredMode', default = False, type = bool)

    if tiredMode:
        subjects = configure_subjects(True)
    else:
        subjects = configure_subjects()
    sorted_subjects = sort_subjects(subjects)
    content = json.dumps(sorted_subjects)

    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/log')
def log():
    cmd = [ os.path.dirname(os.path.realpath(__file__)) + '/../log.sh', '--json']
    content =  subprocess.run(cmd, stdout=subprocess.PIPE).stdout.decode('UTF-8')

    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/done/<subjectName>', methods = ['POST'])
def done(subjectName):

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

    return '{"message": "success"}', 200, {'Content-Type': 'application/json; charset=utf-8'}


@app.route('/detail/<subjectName>')
def detail(subjectName):
    obj = get_subject(subjectName)
    result = json.dumps(obj)
    return result, 200, {'Content-Type': 'application/json; charset=utf-8'}

app.run(host= '0.0.0.0')


