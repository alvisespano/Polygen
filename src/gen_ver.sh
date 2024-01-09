#!/bin/sh


VERSION=$(cat VERSION)
DATE=$(cat DATE)

sed -e "s/%VERSION%/${VERSION}/" -e "s/%DATE%/${DATE}/" ver.ml.template
