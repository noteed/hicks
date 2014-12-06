#! /bin/bash

# Try the life-cycle exposed by the `hicks` executable against UpCloud.
#
# Credentials for UpCloud should be made available in `secret/upcloud-key.txt`.
#
# The hostname `hicks.noteed.com` is really just an example. You don't need
# to control that domain to run this example.
#
# Replace PUBLIC_KEY by an actual public SSH key you have on your local
# machine.

HOSTNAME=hicks.noteed.com
PUBLIC_KEY=/home/thu/.ssh/private_rsa.pub

export PATH=dist/build/hicks/:$PATH

SERVER_ID=$(hicks create ${HOSTNAME})
echo ${SERVER_ID}
hicks wait      ${SERVER_ID} started
hicks password  ${SERVER_ID}
hicks authorize ${SERVER_ID} ${PUBLIC_KEY}

sleep 1
IP=`hicks ip ${SERVER_ID}`
echo ${IP}
ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no root@${IP} 'uname -a'

sleep 1
hicks upload    ${SERVER_ID}
hicks provision ${SERVER_ID}
hicks wait      ${SERVER_ID} Ready
ssh -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no root@${IP} 'cat provision.log'

hicks stop      ${SERVER_ID}
hicks wait      ${SERVER_ID} stopped
hicks delete    ${SERVER_ID}
