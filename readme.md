DOSE CALC service - integrated into GKE

Prerequsites:

1. Install google cloud sdk for you OS and login
Download package from https://cloud.google.com/sdk/docs/

2. Enable GCP API
gcloud services enable container.googleapis.com compute.googleapis.com containerregistry.googleapis.com cloudbuild.googleapis.com

3. Install kubectl

3. Create cluster
gcloud container clusters create CLUSTER_NAME \
    --preemptible \
    --zone  ZONE\
    --scopes cloud-platform \
    --num-nodes 1

Example zone: us-central1-a, more on

!!!SET API CREDENTILS  https://cloud.google.com/docs/authentication/getting-started on a image???
!!! copy ~/.kube/conf (only on linux, no idea where on windows) to server directory

gcloud container clusters get-credentials my-cluster --zone us-central1-a

4. build image of script and service and push to the dockerhub (remember to login to dockerhub and apply corresponding credentials)
    new-server-image-to-dockerhub.sh
    new-script-image-to-dockerhub.sh
4. apply volumes settings (cluster-setings/create_volume.sh)
5. apply deployment of the app
    kubectl apply -f deployment.yaml
6. expose port with service
    kubectl expose deployment flask-deployment --type="NodePort" --port 5000
7. allow traffic on the port that was exposed (external port)
    gcloud compute firewall-rules create my-rule-2 --allow=tcp:PORTNUM

Check if server id running
    curl http:\\NODEIP:PORTNUM
    NODEIP=$(kubectl get nodes -o jsonpath='{ $.items[*].status.addresses[?(@.type=="ExternalIP")].address }')
    (addres of minikube in case of local)


8. Delete cluster
    gcloud container clusters delete my-cluster

# useful links
https://kubernetes.io/docs/tasks/administer-cluster/access-cluster-api/#without-kubectl-proxy
