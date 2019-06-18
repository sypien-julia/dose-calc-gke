#!/bin/bash
docker build -t r-server .
docker tag r-server monikeu/r-app-1:r-server
docker push monikeu/r-app-1:r-server
