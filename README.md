# HTTP Server

## Compile
> make

## Run
> ./start-dev.sh

## Call
> curl -X POST -d @tasks.txt http://localhost/order_tasks or send a POST request to http://localhost/order_tasks from an API Client (Thunder Client, Postman) with the JSON body containing the tasks

## Return
> The tasks in order based on the prerequisites of each task.
> Writes a bash script with the commands from the ordered tasks meaningfully called `script.sh`.
