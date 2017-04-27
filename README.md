# shrdlu
shrdlu is a demonstration of a simple artificial intelligence. It is a conversational agent that
lets you create and interact with a world. You can add/remove/modify objects, ask about objects in
relationship to other objects, query objects with a certain property, and answer yes/no questions.
## Building
If you have everything installed correctly, run `build.sh` then execute `rlwrap ./shrdlu`

## Program Structure
### Main
A small layer of glue to get everything up and running. Implements a simple prompt
for the user to interact with.
### Model
- Objects
  - Properties
    - Color
    - Shape
    - maybe more? should be implemented to be extensible. Maybe with use of predicates?
  - Location
### Parser
- Modifying Model
  - Creation
  - Deletion
  - Change property/position
  - Examples
    - Create a blue sphere anywhere
    - Place a red cube next to the blue sphere
    - Place a red cube next to a blue sphere
    - Delete all the red objects
- Query
  - Ask about a property, a position, or a relative position
  - Examples
    - Are there any objects that are red and a cube?
    - What is the color of all the cubes?
    - What is the color of some of the cubes?
    - Are there at least 2 cubes to the right of the red sphere?
- Decision Questions
  - Is there a red cube?
  - Is there a blue cylinder on top of the red sphere?
- Relations / Relative clauses
  - The above examples use relative statements. Such as `on top of`, `to the right of`, etc.
  - Maybe want to deal with pronouns?

