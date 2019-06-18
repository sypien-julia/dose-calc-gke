# dose-calc-gke - integrated into GKE

How to run the app on GKE:

1. Install google cloud sdk for you OS and login
Download package from https://cloud.google.com/sdk/docs/

2. Enable GCP API  
`gcloud services enable container.googleapis.com compute.googleapis.com containerregistry.googleapis.com cloudbuild.googleapis.com`

3. Install kubectl

4. Create cluster  
`gcloud container clusters create CLUSTER_NAME    
    --preemptible  
    --zone  ZONE  
    --scopes cloud-platform  
    --num-nodes 1`
    
Example zone: us-central1-a, more on https://cloud.google.com/compute/docs/regions-zones/

Execute:  
`gcloud container clusters get-credentials CLUSTER --zone ZONE`

5. Download API key from    
https://cloud.google.com/docs/authentication/getting-started and place it in `server/credentials.json`,   it will be needed for the image,
6. Copy contents of `~/.kube/conf` (only on linux) file to server directory to `/server/kubeconfig`. Also needed for the image.
Point 5 and 6 are must-have!

7. Build image of script and service and push to the Dockerhub (remember to login to dockerhub).  
    
    `r-image/new-image-to-dockerhub.sh`  
    `server/new-server-image-to-dockerhub.sh`  
  
   are just templates, remember to adjust them to your account. Also as the server image has to be rebuilt (because of the credentials) you need to adjust deployment.yaml file to download from your repository 
    
4. Create volumes on GKE.
    `kubectl apply -f /templates/pvc.yaml`

5. Create deployment with server
    `kubectl apply -f /templates/deployment.yaml`

6. Expose port with service for the external traffic
    `kubectl expose deployment flask-deployment --type="NodePort" --port 5000`
    
7. Allow traffic on the port that was exposed (external port) on GCE firewall
   `gcloud compute firewall-rules create my-rule --allow=tcp:PORTNUM`
   
   you can check the port number with   
   `kubectl get services`  

8. Check if server is running
   `curl http:\\NODEIP:PORTNUM` where
   `NODEIP=$(kubectl get nodes -o jsonpath='{ $.items[*].status.addresses[?(@.type=="ExternalIP")].address }')`

Horray! The server is working!

If you want to delete cluster run   
 `gcloud container clusters delete CLUSTER_NAME`

