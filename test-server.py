from flask import Flask, request, abort, send_file
# import json
from google.cloud import container_v1
import kubernetes.client
#
# client = container_v1.ClusterManagerClient()
#
# project_id = 'dose-test'
# zone = 'us-central1-a'
# cluster = {
#     "name": "dose-calc-cluster",
#     "description": "Cluster for dose-calc-gke app",
#     "initial_node_count": 1
# }
# response = client.create_cluster(project_id, zone, cluster)
# print(response)

app = Flask(__name__)


@app.route('/')
def index():
    return 'Doze calculator'


# @app.route('/jobs', methods=['POST'])
# def scheduleJobs():
#     jobsParametersArr = json.loads(request.json)
#     for parameters in jobsParametersArr:
#         if ("oralDose" not in parameters
#                 or "infDose" not in parameters
#                 or "infTime" not in parameters
#                 or "individualCount" not in parameters
#                 or "femaleCount" not in parameters
#                 or "minAge" not in parameters
#                 or "maxAge" not in parameters
#                 or "tEnd" not in parameters
#                 or "seed" not in parameters):
#             return abort(422,
#                          'Invalid arguments')
#
#     return 'Created one or more jobs', 201


@app.route('/download/<jobId>')
def downloadFile(jobId):
    path = "/mydata/" + jobId + ".txt"
    return send_file(path, as_attachment=True)


# @app.route('/jobs/<int:jobId>', methods=['DELETE'])
# def deleteJob(jobId):
#     if True:
#         return 'Job cancelled', 200
#     elif True:
#         return 'No such job', 404
#     else:
#         return 'Job is not running', 405


# @app.route('/jobs/<int:jobIds>/status', methods=['GET'])
# def getJobStatus(jobIds):
#     if True:
#         jobId = 0
#         status = "running"
#         response = app.response_class(
#             response=json.dumps(
#                 [
#                     {
#                         "id": jobId,
#                         "status": status
#                     }
#                 ]
#             ),
#             status=200,
#             mimetype='application/json'
#         )
#         return response
#     else:
#         return 'No such jobs', 404
#
#
# @app.route('/jobs/<int:jobIds>/results', methods=['GET'])
# def getJobResults(jobId):
#     if True:
#         jobId = 0
#         populationUrl = "populationUrl"
#         newDfUrl = "newDfUrl"
#         response = app.response_class(
#             response=json.dumps(
#                 [
#                     {
#                         "id": jobId,
#                         "errorOccurred": False,
#                         "population": populationUrl,
#                         "newDf": newDfUrl,
#                     }
#                 ]
#             ),
#             status=200,
#             mimetype='application/json'
#         )
#         return response
#     else:
#         return 'No such jobs', 404
#
#
# if __name__ == '__main__':
#     app.run()

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=5000)