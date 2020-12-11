# Habit Screens

## MVP

One Android tablet mounted on my bedroom wall displaying habits for that day. I
can toggle the done/todo states on each item by tapping it. There is no
server. All of the habits are defined in the client-side codebase. The
application is available online at wpcarro.dev.

## Ideal

Three Android tablets: one mounted in my bedroom, another in my bathroom, and a
third in my kitchen. Each tablet has a view of the current state of the
application and updates in soft real-time.

I track the rates at which I complete each habit and compile all of the metrics
into a dashboard. When I move a habit from Saturday to Sunday or from Wednesday
to Monday, it doesn't break the tracking.

When I complete a habit, it quickly renders some consistency information like
"completing rate since Monday" and "length of current streak".

I don't consider this application that sensitive, but for security purposes I
would like this application to be accessible within a private network. This is
something I don't know too much about setting up, but I don't want anyone to be
able to visit www.BillAndHisHabits.com and change the states of my habits and
affect the tracking data. Nor do I want anyone to be able to make HTTP requests
to my server to alter the state of the application without my permission.

## Client

Language: Elm

### Updates across devices

Instead of setting up sockets on my server and subscribing to them from the
client, I think each device should poll the server once every second (or fewer)
to maintain UI consistency.

## Server

Language: Haskell
Database: SQLite
