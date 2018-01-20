#!/usr/bin/env python


from scheduler import configure_subjects, sort_subjects
from flask import Flask, request, jsonify
import json

app = Flask(__name__)
app.debug=True

@app.route('/')
def index():
    subjects = configure_subjects()
    sorted_subjects = sort_subjects(subjects)
    return json.dumps(sorted_subjects)

app.run(host= '0.0.0.0')
