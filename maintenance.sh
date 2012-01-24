#!/bin/bash

sudo /sbin/iptables -t nat -I PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8081
echo "Porta 80 mappata sulla 8081 (sito in manutenzione)"

