---
layout: post
title:  "A microservices architecture approach"
date:   2015-01-12
---

The following blog post outlines a microservices architecture which aims to be very minimalistic and easy to use, modify and deploy, to serve as a testbed from where more sophisticated designs can be developed. No thought will be given to any technical prowess, such as speed of interservice communication.

'Microservices', a rebranded form of service oriented architecture (SOA) is becoming popular again, for the following reasons:
- the stack becomes flexible, most notable, multiple languages can be used to build the same piece of software
- deployment is eased by breaking down the codebase into smaller, independent components
- since subcomponents can be deployed independently, they can react to load needs independently, it is just a matter of spinning up more instances of a subcompontent.

The 'micro' in the name hints at the desire of the service writers to keep each service small. As small as possible would be a good guideline but this needs sufficient tooling. Having a streamlined service creation and deployment process encourages developers to not build bigger services than needed.

In the following a design from simpler to more complex will be explained

### How a service looks like

#### From the outside (clients)

A service for the service caller does not have to be more than a collection of functions accepting and returning some data. To make this as accessible as possible, for the outsider, a service must appear to be an HTTP server speaking JSON:

{% highlight bash %}
$ curl http://127.0.0.1:8081/sayHello -d '{
    "name": "John"
}'
$ {"says": "Hello, John"}
{% endhighlight %}

There is no probably no language or environment used by modern application developers which has no support for HTTP and JSON.

#### From other services

As a start, no code generation will be introduced in this design to keep matters simple.
Service calls can happen with a single line of code:

{% highlight javascript %}
var rsp = call("serviceName.EndpointName", {
    "sayHello": "John"
});
{% endhighlight %}

This poses a problem in typed languages: every service call becomes type unsafe - we do not now wether the service, or the endpoint exists, or what is the structure of input and output arguments. This will be addressed later.

##### What happens when a service calls an other

Looking at the previous code snippets, we can see that the shell CURL talked to a specific HTTP server, while the JavaScript snippet referred to a service by name. A bookeeping entity, which maps service names to service instances must be introduced.

There is a working prototype of such a entity in a form of a service [here](https://github.com/crufter/orc).
Here is how it works:

There is an endpoint for registering a service instance under a certain name (assuming 127.0.0.1:8082 is the address of such a bookkeeping service running):

{% highlight bash %}
curl http://127.0.0.1:8082/connect -d '{
    "address": "127.0.0.1:8081",
    "serviceName": "helloSayer",
    "instanceName": "helloSayer 1",
    "endpoints": {
        "sayHello": {
            "alias": "sayHello",
            "path": "sayHello"
        }
    }
}'
{% endhighlight %}

After this point, anyone who CURLs this service like this:

{% highlight bash %}
curl http://127.0.0.1:8082/helloSayer/sayHello -d '{
    "name": "John"
}'
{% endhighlight %}

Will be proxied to our 'helloSayer' service. When service A calls service B, our communication will look like the following:

{% highlight javascript %}
 ___________              ____________              ___________
|           |  Request   |            |  Request   |           |
|           | ---------> |            | ---------> |           |
| Service A |  Response  | Bookkeeper |  Reponse   | Service B |
|           | <--------- |            | <--------- |           |
|___________|            |____________|            |___________|
{% endhighlight %}

As it can be seen, the bookeeper also serves as a proxy. Since all communications go trough this service, it can also act as a load balancer, it could analyze response times, smart route requests accordingly, and could provide monitoring information about the services on the network.


### Services as interfaces - a protocol

The number of microservices can get rather large - to efficiently manage a large sized cluster we can not afford services to be special snowflakes - they must be as alike as possible. There is no point in having ReadById or similar endpoints having different naming conventions. Apart from the obvious ones like that, we can use this concept to take things one step further...

#### Reading data from multiple services

A good (although somewhat controversial) candidate for service interfaces would be the way we access data from services. Boring CRUD-like data listing endpoints could entirely automated away by a system which reads all data from all services and provides ways to query that data. The only requirement for this is to have a way to easily copy the data set stored in services and to keep the replica in sync with the original.

A solution to this would be to have an 'ListByUpdateDesc' or similar interface, which would be implemented by a service by listing every stored entity by last update(/create) time in a reverse chronological order:

{% highlight bash %}
$ curl http://customers-service.myservices.com/customers/listByUpdateDesc -d '{
    "fromTime": "2015 01 27 18:23:55",
    "limit":    5
}'
$ {
    "customers": [
        {
            "id":           "100",
            "name":         "Joe",
            "favFruit":     "apple",
            "lastUpdated":  "2015 01 27 18:23:59"
        },
        {
            "id":           "28",
            "name":         "Jane",
            "favFruit":     "banana",
            "lastUpdated":  "2015 01 27 18:23:57"
        },
        {
            "id":           "182",
            "name":         "Jill",
            "favFruit":     "blueberry",
            "lastUpdated":  "2015 01 27 18:23:57"
        },
        {
            "id":           "41",
            "name":         "Jacob",
            "favFruit":     "apple",
            "lastUpdated":  "2015 01 27 18:23:57"
        },
        {
            "id":           "55",
            "name":         "John",
            "favFruit":     "peach",
            "lastUpdated":  "2015 01 27 18:23:55"
        },
    ]
}
{% endhighlight %}

### Making services even smaller

To be continued.