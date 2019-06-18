#!/bin/bash
docker build -t r-image-env .
docker tag r-image-env monikeu/r-script-1:r-image-env
docker push monikeu/r-script-1:r-image-env