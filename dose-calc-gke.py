from flask import Flask, request, abort
import json
from google.cloud import container_v1
from kubernetes import client as kubeClient
from kubernetes import config
from kubernetes.client import V1EnvVar
import uuid

gkeClient = container_v1.ClusterManagerClient()

project_id = 'dose-calc'
zone = 'us-central1-b'
cluster = {
  "name": "dose-calc-cluster",
  "description": "Cluster for dose-calc-gke app",
  "initial_node_count": 3
}
# response = gkeClient.create_cluster(project_id, zone, cluster)
# exec("gcloud container clusters get-credentials dose-calc-cluster3") not working :(

config.load_kube_config()
configuration = kubeClient.Configuration()

api_instance = kubeClient.AdmissionregistrationApi()

api_instance = kubeClient.BatchV1Api(
    kubeClient.ApiClient(configuration))
api_pods = kubeClient.CoreV1Api(
    kubeClient.ApiClient(configuration))


# v1 = kubeClient.CoreV1Api()
# print("Listing pods with their IPs:")
# ret = v1.list_pod_for_all_namespaces(watch=False)
# for i in ret.items:
#     print("%s\t%s\t%s" % (i.status.pod_ip, i.metadata.namespace, i.metadata.name))

app = Flask(__name__)

@app.route('/')
def index():
    return 'Dose calculator'

@app.route('/jobs', methods=['POST'])
def scheduleJobs():
    for jobParameters in request.json:
        if not validateJobParameters(jobParameters):
            return abort(422,
                         'Invalid arguments')

        body = kubeClient.V1Job(api_version="batch/v1", kind="Job")
        # Body needs Metadata
        # Attention: Each JOB must have a different name!
        jobName = "r-job-" + str(uuid.uuid4())
        body.metadata = kubeClient.V1ObjectMeta(namespace="default", name=jobName)
        # And a Status

        body.status = kubeClient.V1JobStatus()
        # Now we start with the Template...
        template = kubeClient.V1PodTemplate()
        template.template = kubeClient.V1PodTemplateSpec()
        # Passing Arguments in Env:

        env_list = createJobEnv(jobParameters, jobName)

        volume_mounts = kubeClient.V1VolumeMount(mount_path="/mydata", name="host-volume")
        container = kubeClient.V1Container(name="r-container", image="monikeu/r-script-1:r-image-env",
                                           env=env_list, volume_mounts=[volume_mounts])
        per_vol_claim = kubeClient.V1PersistentVolumeClaimVolumeSource(claim_name="pvc-hostpath")
        volume = kubeClient.V1Volume(name="host-volume", persistent_volume_claim=per_vol_claim)
        template.template.spec = kubeClient.V1PodSpec(containers=[container],
                                                      restart_policy='Never',
                                                      volumes=[volume])
        # And finaly we can create our V1JobSpec!
        body.spec = kubeClient.V1JobSpec(ttl_seconds_after_finished=600,
                                         template=template.template)

        response = api_instance.create_namespaced_job("default", body, pretty=True)

        print(response)

    return 'Created one or more jobs', 201

def validateJobParameters(jobParameters):
    if ("oralDose" not in jobParameters
        or "infDose" not in jobParameters
        or "infTime" not in jobParameters
        or "individualCount" not in jobParameters
        or "femaleCount" not in jobParameters
        or "minAge" not in jobParameters
        or "maxAge" not in jobParameters
        or "tEnd" not in jobParameters
        or "seed" not in jobParameters):
            return False
    return True


def createJobEnv(jobParameters, jobName):
    env_list = []
    env_vars = [V1EnvVar("ORAL_DOSE", jobParameters["oralDose"]),
                V1EnvVar("INF_DOSE", jobParameters["infDose"]),
                V1EnvVar("INF_TIME", jobParameters["infTime"]),
                V1EnvVar("INDIVIDUAL_COUNT", jobParameters["individualCount"]),
                V1EnvVar("FEMALE_COUNT", jobParameters["femaleCount"]),
                V1EnvVar("MIN_AGE", jobParameters["minAge"]),
                V1EnvVar("MAX_AGE", jobParameters["maxAge"]),
                V1EnvVar("T_END", jobParameters["tEnd"]),
                V1EnvVar("SEED", jobParameters["seed"]),
                V1EnvVar("JOB_NAME", jobName)]

    for x in env_vars:
        env_list.append(kubeClient.V1EnvVar(name=x.name, value=x.value))
    return env_list



@app.route('/jobs/<int:jobId>', methods=['DELETE'])
def deleteJob(jobId):
    if True:
        return 'Job cancelled', 200
    elif True:
        return 'No such job', 404
    else:
        return 'Job is not running', 405


@app.route('/jobs/<int:jobIds>/status', methods=['GET'])
def getJobStatus(jobIds):
    if True:
        jobId = 0
        status = "running"
        response = app.response_class(
            response=json.dumps(
                [
                    {
                        "id": jobId,
                        "status": status
                    }
                ]
            ),
            status=200,
            mimetype='application/json'
        )
        return response
    else:
        return 'No such jobs', 404


@app.route('/jobs/<int:jobIds>/results', methods=['GET'])
def getJobResults(jobId):
    if True:
        jobId = 0
        populationUrl = "populationUrl"
        newDfUrl = "newDfUrl"
        response = app.response_class(
            response=json.dumps(
                [
                    {
                        "id": jobId,
                        "errorOccurred": False,
                        "population": populationUrl,
                        "newDf": newDfUrl,
                    }
                ]
            ),
            status=200,
            mimetype='application/json'
        )
        return response
    else:
        return 'No such jobs', 404

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
