# Group C project : A Distributed Heating Trigger System

# How to launch our application : 

To launch our application, with simulated nodes, open a terminal inside the achlys folder and enter the command "make shell $n=1$ $PEER\_PORT=27001$". 
This will create a first emulated node and launch our application on it. The created shell will start by showing something like "application launched on node achlysX@130.104.164.101". You should note this IP address, we will need it later (let's call it MyIp). If you want, you can already check the outputs inside this terminal. You can notice that the node is detecting that it is alone and already considers itself as the leader. 

Then you can launch a second node by opening a new terminal inside the achlys folder and enter the command "make shell $n=2$ $PEER\_PORT=27002$". For now, this node is also alone and will behave as the previous one, considering itself as a leader. Then inside this shell, enter the command $"lasp\_peer\_service:join('achlys1@130.104.164.101')"$ where you should replace 130.104.164.101 by the real value you got for MyIp (see above). 
You have now two nodes clustered together, they will detect each other (output shows " 2 valid nodes") and elect Node1 as the leader.

Here you are, to create a bigger cluster of nodes, you can repeat the same procedure inside new terminals with the same commands as before "make shell n=X $PEER\_PORT=2700X$" where X is the number of the board (1,2,3,4,5... up to 10) then the join command inside each created shell. 

We made the outputs inside these shells as clear as possible to make it easy to understand the actual state of the application. 

Many more informations are available in our report "report_group_c.pdf" directly inside this folder.
