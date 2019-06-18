from __future__ import print_function
import time
import kubernetes.client
from kubernetes.client import V1EnvVar
from kubernetes.client.rest import ApiException
from kubernetes import client, config
from pprint import pprint

config.load_kube_config("/home/pboszczy/.kube/config")
configuration = kubernetes.client.Configuration()

api_instance = kubernetes.client.AdmissionregistrationApi()

api_instance = kubernetes.client.BatchV1Api(
    kubernetes.client.ApiClient(configuration))
api_pods = kubernetes.client.CoreV1Api(
    kubernetes.client.ApiClient(configuration))

# Body is the object Body
body = client.V1Job(api_version="batch/v1", kind="Job")
# Body needs Metadata
# Attention: Each JOB must have a different name!
body.metadata = client.V1ObjectMeta(namespace="default", name="r-job-10")
# And a Status

body.status = client.V1JobStatus()
# Now we start with the Template...
template = client.V1PodTemplate()
template.template = client.V1PodTemplateSpec()
# Passing Arguments in Env:

env_vars = [V1EnvVar("ORAL_DOSE", "880"), V1EnvVar("INF_DOSE", "0"), V1EnvVar("INF_TIME", "2"), V1EnvVar("T_END", "15"),
            V1EnvVar("SEED", "1111"), V1EnvVar("JOB_NAME", "1212121")]
env_list = []
for x in env_vars:
    env_list.append(client.V1EnvVar(name=x.name, value=x.value))

volume_mounts = client.V1VolumeMount(mount_path="/mydata", name="dose-volume")
container = client.V1Container(name="r-container", image="monikeu/r-script-1:r-image-env",
                               env=env_list, volume_mounts=[volume_mounts], image_pull_policy="Always")
per_vol_claim = client.V1PersistentVolumeClaimVolumeSource(claim_name="dose-volume-claim")
volume = client.V1Volume(name="dose-volume", persistent_volume_claim=per_vol_claim)
template.template.spec = client.V1PodSpec(containers=[container],
                                          restart_policy='Never',
                                          volumes=[volume])
# And finaly we can create our V1JobSpec!
body.spec = client.V1JobSpec(ttl_seconds_after_finished=600,
                             template=template.template)

response = api_instance.create_namespaced_job("default", body, pretty=True)

print(response)
