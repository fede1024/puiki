#!/bin/bash

sudo /sbin/iptables -t nat -I PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080
echo "Porta 80 mappata sulla 8080 (sito online)"

