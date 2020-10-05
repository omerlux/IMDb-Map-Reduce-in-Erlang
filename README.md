# Project-Ilay-Omer - Mapreduce IMDb in erlang
 This is the final project of "Functional programming in concurrent and distributed systems", by Ilay and Omer
 
- ### General Info:
 
 The main purpose of the project is to implement a map-reduce algorithm for iMDB. 
 
 The data is taken from [Kaggle](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset) and contains ~85k records of movies.
 In addition, we created a distributed machine which distributes the data from *master* node to *servers* node. Number of servers can be chosen by the user.
 
 *clients* can be created in any computer/terminal and ask for different kinds of queries from the *master* node. The answer will be delivered from each *server* (which contains different records), to the *master* and then the master will group them into one result which will be sent back to the client.

- ### Technical Overview:
We built the system as the Disco-Project system supposed to be:
<p style="height: 20px; overflow: hidden" align="center">
  <img src="https://disco.readthedocs.io/en/latest/_images/disco-arch.png">
</p>

##### Our overview of the system (Same as [Disco-Project](https://disco.readthedocs.io/en/latest/overview.html))

- ### How to Run:

 To run ***server***, write the server's name in *@serverslist.txt* - *server#@ip*
 then at each server write the following line on terminal:
```
erl -name server# -run server start_link
```

 To run ***master***, write the master's name in *@clientslist.txt* (on the first line) - *master@ip*
 then at master terminal write the following line on terminal:
```
erl -name master -run master start_link
```

 To run ***client*** (which can be on the same computer as the servers)
 then at client terminal write the following line on terminal:
```
erl -name client# -run wxclient start
```


 **(-name can be -sname for local connection run)**

- ### Progress:
- [x] Build infrastructure
- [x] Handle exceptions
- [ ] Finish client's gui
- [ ] Add statistics to queries
