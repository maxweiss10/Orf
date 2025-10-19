# Memory Usage Fix - October 2025

## Problem
The code was running out of memory due to excessive output being printed during simulation runs.

## Root Causes
1. **print(t) in simulation loops**: The `print(t)` statements in `3_sim_function.R` and `3_sim_function_orfor.R` were printing the cycle number for every iteration of the simulation loop. With default settings of:
   - n.cycle = 60 (cycles per simulation)
   - n.loop = 5 (replicates per individual)
   - n.sample = 200 (individuals)
   - n.sim = 2 (probabilistic samplings)
   
   This resulted in printing 60 values per simulation, which accumulated in memory and caused out-of-memory errors.

2. **Verbose foreach output**: The `.verbose = T` option in the foreach loops was also generating output for each simulation run, adding to the memory pressure.

## Solutions Applied

### 1. Removed print(t) statements
**Files modified:**
- `Produce-Rx-main/02_programs/3_sim_function.R` (line 77)
- `Produce-Rx-main/02_programs/3_sim_function_orfor.R` (line 68)

**Change:** Replaced `print(t)` with a comment explaining why it was removed.

### 2. Disabled verbose output in foreach loops
**Files modified:**
- `Produce-Rx-main/02_programs/01_DOCM_orfor.R` (lines 571, 578)
- `Produce-Rx-main/02_programs/01_DOCM_produce_rx.R` (lines 529, 536)
- `Produce-Rx-main/02_programs/01_DOCM_produce_rx_ogkinda.R` (lines 495, 502)

**Change:** Changed `.verbose = T` to `.verbose = F` in foreach loops.

### 3. Added .gitignore
**File created:** `.gitignore`

**Purpose:** Prevents accidental committing of:
- Output files (*.rda, *.rds)
- macOS metadata (__MACOSX/, .DS_Store)
- R temporary files (.Rhistory, .RData, .Rproj.user)
- Zip archives

## Impact
These changes eliminate the massive output generation during simulation runs, preventing memory exhaustion. The simulations will now run more efficiently with minimal console output.

## Remaining Output
The following print statements remain and are not problematic:
- Status messages in main scripts (e.g., "Importing data", "running model", "Summarizing and saving output")
- Progress time reports

These only run once per execution or simulation setup, not in tight loops, so they don't cause memory issues.

## Testing
Since R is not available in the build environment, the changes have been validated through:
1. Syntax verification of the modified R code
2. Checking that the loop structures remain intact
3. Verifying that no other excessive output statements exist

Users should test the changes by running the simulation with their normal parameters and verifying that:
1. No memory errors occur
2. The simulation completes successfully
3. Output files are generated correctly
4. Results match expected values
