#!/bin/bash

# crop all pdfs in folder to minimum sizes 
for FILE in ./*.pdf; do
  pdfcrop "${FILE}" 
done
