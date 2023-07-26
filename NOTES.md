# Notes for the project

## Timeline

- ~~Kickoff meeting~~
- ~~Lit review~~
- Develop scenarios
- Methodology
- Analysis
- Conclusions

## Develop scenarios

Things that are needed for scenario development:

### General

#### ActivitySim

- How to get synthetic population / Which variables to use from PUMS?
- How to use skims from WFRC model / Which skims to use?
- How to input transit?
    - Options:
        - TAZ Skims (get from CUBE)
        - TAPs and TAP skims (do we use these?)
- How to input WFH?
    - Built-in WFH model
        - Separate models for fully WFH and blended
        - Need to make model spec, copy from WFRC?
    
#### WFRC

- How to input transit?
    - We found the transit file(s)
    - Can easily change headway, but the numbers (zone?) are confusing
- How to adjust WFH?
    - WFRC has built this in to their model a bit
    - How did they do that?
    - How specifically do we adjust it?
    
### Scenario-specific

#### Prison redevelopment

- What to change SE data to?
    - WFRC has projections, should we use those?
- Add BRT as planned?
    - WFRC probably has this, need to check
    
#### FrontRunner double-tracking

- Change speed?
    - In WFRC model, use network 'speed' value
    - How to do in ActivitySim?

#### WFH

- Different % for different job types
    - Edit model spec in ActivitySim
    - This is probably built in to WFRC?
- Discretionary trips go up?
    - How to do this

## Methodology

**TODO**

## Analysis

**TODO**

## Conclusions

**TODO**
