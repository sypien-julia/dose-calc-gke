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
6. Copy contents of `~/.kube/conf` file to server directory to `/server/kubeconfig`. Also needed for the image.
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


# Working with the app #

You can use the app to:


1. Initialize your calculations

* url: `/jobs`
* method: `POST`
* request body: json array containing dictionaries with calculations parameters, one dictionary for one calculation

Example:
```json
[
  {
    "oralDose": "3",
    "infDose": "2",
    "infTime": "3",
    "individualCount": "10",
    "femaleCount": "3",
    "minAge": "3",
    "maxAge": "90",
    "tEnd": "0",
    "seed": "0"
  }
]
```

* returns: 201 code and a message with created jobs' ids on success, error code on failure


2. Delete calculation

* url: `/jobs/<string:jobId>`
* method: `DELETE`
* parameter: returned by /jobs method id of the job to be deleted
* returns: 201 code on success, error code on failure


3. Get calculations statusses

* url: `/jobs/<path:jobIds>/status`
* method: `GET`
* parameter: returned by /jobs method ids of the jobs, separated with "/" mark
* returns: 200 code and a json with job statusses on success, error code on failure


4. Get calculation results

* url: `/download/<jobId>`
* method: `GET`
* parameter: returned by /jobs method id of the job the results of which you want do download
* returns: files with the results
