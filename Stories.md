These User Stories will be used to develop new features. For the most work, new code
should be motivated by a Story from this list.

# Currently active Story(ies)

**AS A** MyCAD Server User \
**I WANT** to send requests directly to a running server instance locally \
**SO THAT** I can understand how to interact with the Server

# Stories backlog
## Create geometric objects
**AS A** Desktop User \
**I WANT** to use my mouse to make geometric objects \
**SO THAT** I can visualize how these objects relate to each other \

**AS A** Remote User \
**I WANT** to send a request to make Vectors and Lines using specified geomtric information \
**SO THAT** I can later manipulate those objects in space

**AS A** Power User \
**I WANT** to use python to make geometric objects \
**SO THAT** I can automate my work flow. \

## Create topological objects

**AS A** Designer \
**I WANT** to create a Face from a collection of Edges \
**SO THAT** I can eventually use it to make some sort of Solid \

**AS A** Designer \
**I WANT** to create a Primitive Solid \
**SO THAT** I can use it to describe some physical object \

## Work with groups of topological objects
**AS A** Designer \
**I WANT** to group various topological entities in a "Document" \
**SO THAT** I can work on them iteratively and encapsulate them from other logical groups

## Communicate with MyCAD::Communication::Server
**AS A** MyCAD Server Administrator \
**I WANT** to communicate directly with a MyCAD::Server process \
**SO THAT** I can stop the process

**AS A** MyCAD Server Administrator \
**I WANT** allow a remote User to have a specific login Session \
**SO THAT** their work is sandboxed from any other User's work.

# Completed Stories
**AS A** Designer \
**I WANT** to create lines in space \
**SO THAT** I can use them to make more complex geometry \

**AS A** Remote User \
**I WANT** to send requests to MyCAD server using TCP/IP \
**SO THAT** I can recieve responses and use the calculated geometry \
