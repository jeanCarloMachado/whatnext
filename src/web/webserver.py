#!/usr/bin/env python
import os, sys
import subprocess

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from scheduler import configure_subjects, sort_subjects

from flask import Flask, request, jsonify, Response
from flask_cors import CORS
import json

app = Flask(__name__)
app.debug=True
CORS(app)

@app.route('/')
def index():
    subjects = configure_subjects()
    sorted_subjects = sort_subjects(subjects)
    content = json.dumps(sorted_subjects)

    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/log')
def log():
    cmd = [ os.path.dirname(os.path.realpath(__file__)) + '/../log.sh', '--json']
    content =  subprocess.run(cmd, stdout=subprocess.PIPE).stdout.decode('UTF-8')

    return content, 200, {'Content-Type': 'application/json; charset=utf-8'}

@app.route('/detail/<subject>')
def detail(subject):
    cmd = [ os.path.dirname(os.path.realpath(__file__)) + '/../log.sh', '--filter', subject, '--json']
    subjectHistoryStr =  subprocess.run(cmd, stdout=subprocess.PIPE).stdout.decode('UTF-8')
    subjectHistory = json.loads(subjectHistoryStr) 

    obj = {"subject": subject, "history": subjectHistory}
    result = json.dumps(obj)

    return result, 200, {'Content-Type': 'application/json; charset=utf-8'}

app.run(host= '0.0.0.0')
