
"""

Implicit texture segregation during inattentional blindness (replication)

Alterations:
    Inserted markers for EEG testing

Adapted from Pitts, Martinez and Hillyard (2011)

"""

from psychopy import visual, gui, core, event, data
import os
import numpy as np
import datetime
import random
import ctypes as ctypes
import pdb
import time
from PIL import Image
now = datetime.datetime.now()


#################### ##################################################
######!!!CHECK THE SETTINGS BELOW BEFORE YOU START TESTING!!!!######  
######################################################################

### CHECK SCREEN RESOLUTION IN EEG ROOM (1800x1440)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!######
EEG = 0 # Set to 1 for experiment (0 is for try out mode)
pixDeg = 35.39703143518257
numTrials = 61
Screen = 1 # 0 if main screen; 1 if test screen
FullScreen = True # True if fullscreen, False if not

######################################################################
######!!!CHECK THE SETTINGS BELOW BEFORE YOU START TESTING!!!!######
######################################################################

if EEG == 1:
    PCIDAQ24 = ctypes.WinDLL('C:\Program Files (x86)\Measurement Computing\DAQ\cbw32.dll');
    BoardNum = 0;
    PortNum = 11; # Works only with this port number for now
    DIGITALOUT = 1 # Mode of operation, needs to be set to 1
    b = PCIDAQ24.cbDConfigPort(BoardNum, PortNum, DIGITALOUT)

# Matrix
rows = 70
cols = 90
nLines = cols * rows
lineLength = 0.306 * pixDeg
lineWidth = 2
lineSize = ([lineLength, lineWidth])
pos = np.ones([cols*rows, 2])
distsCenter = np.empty(cols*rows)
matrixContrasts = np.empty(cols*rows)
lineContrDecrem = 1.0/(rows/2 - 10) # divides contrast spam by the number of columns on each side minus rowsInRing
rowsInRing = 10 # number of rows/columns inside ring, which have constant contrast (don't count for purposes of fading)
# Use to manipulate density of lines:
xs = 1.5 * 0.308421052631579 * pixDeg; # x spacing
ys = 1.5 * 0.308421052631579 * pixDeg; # y spacing
# Use to set location of grid
xpos = 0.5; # set L/R location of grid
ypos = 0.5; # set U/D location of grid (0.5 = central)
# List to keep track of line orientation in last column (by indexing by number of row)
p = range(0, rows)
# List to keep track of previous diagonal position
prevDiag = range(0, rows)

# Ring and discs
radRingDeg = 4.9
radRing = 4.9 * pixDeg
sizeDiscsDeg = 1
sizeDiscs = pixDeg
discBaseColor = (1, -1, -1)
ringBaseColor = (1, -1, -1)
discBaseContrast = 1.0
feedbackTime = 0.066

expName = 'Implicit segregation IB'
expInfo = {'Participant number': ''} 
expInfo2 = {'Phase number':''} 
gui.DlgFromDict(dictionary=expInfo, title=expName)
while expInfo2['Phase number'] not in ['0', '1', '2', '3']:
    gui.DlgFromDict(dictionary=expInfo2, title=expName)

fname = os.getcwd() + os.path.sep + 'Data' + os.path.sep + '%s_%s_%s' %(expName,expInfo['Participant number'], expInfo2['Phase number']) 
f = open(fname + ".txt", 'a') 
towrite = str(now) + "\n" + "Number of rows =" + "\t" + str(rows) + "\n" + "Number of columns =" + "\t" + str(cols) + "\n"  + \
"Number of line segments =" + "\t" + str(nLines) + "\n"  + "Line length =" + "\t" + str(lineLength) + "\n"  + "Line width =" + "\t" + \
str(lineWidth) + "\n" + "Ring radius in degrees =" + "\t" + str(radRingDeg) + "\n" + "Radius of discs in degrees =" + "\t" + str(sizeDiscsDeg) + "\n" + \
"Disc base color =" + "\t" + "red" + "\n" + "Ring color =" + "\t" + "red" + "\n" + "Discs base contrast =" + "\t" + str(discBaseContrast) + "\n\n" + "SubID" + \
"\t" + "Phase" + "\t" + "Block" + "\t" + "Trial" + "\t" + "Target presence" + "\t" + "Configuration" + "\t" + "Decrement" + "\t" + "RT" + "\t" + "Resp" + "\t"\
+ "correct"
f.write(towrite + "\n")

# Setup window
win = visual.Window([1280,800], color= (-1, -1, -1), units='pix',allowGUI = False, fullscr = FullScreen, colorSpace = 'rgb', screen = Screen)

# Creates discs for main task
disc0 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = (radRing, 0), size = sizeDiscs)
disc1 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = ((np.cos(np.pi/4)*radRing), (np.sin(np.pi/4)*radRing)), size = sizeDiscs)
disc2 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = (0, radRing), size = sizeDiscs)
disc3 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = ((-(np.cos(np.pi/4)*radRing)), (np.sin(np.pi/4)*radRing)), size = sizeDiscs)
disc4 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = (-radRing, 0), size = sizeDiscs)
disc5 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = ((-(np.cos(np.pi/4)*radRing)), (-(np.sin(np.pi/4)*radRing))), size = sizeDiscs)
disc6 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = (0, -radRing), size = sizeDiscs)
disc7 = visual.GratingStim(win, color = discBaseColor, colorSpace = 'rgb', contrast = discBaseContrast, tex = None, mask = 'circle', \
pos = ((np.cos(np.pi/4)*radRing), (-(np.sin(np.pi/4)*radRing))), size = sizeDiscs)


# Creates outer ring
ring = visual.Circle(win, fillColor= None, lineColor = ringBaseColor, lineWidth = 5, radius = 4.9 * pixDeg)

# Creates fixation cross
fix1 = visual.GratingStim(win, tex = None, mask = None, color = ringBaseColor, size = (2.0, 14.48))
fix2 = visual.GratingStim(win, tex = None, mask = None, color = ringBaseColor, size = (14.48, 2.0))

# Creates feedback circle
feedback1 = visual.Circle(win, fillColor= None, lineColor='red', lineWidth = 2, radius = 8)
feedback2 = visual.Circle(win, fillColor= None, lineColor='green', lineWidth = 2, radius = 8)

# Creates text for breaks
breakText = visual.TextStim(win, text = 'Take a break.\nPress spacebar to resume when ready.', height = 40)
endPhaseText = visual.TextStim(win, text = 'OK, take a break for a few minutes.', height = 40)

# Generates positions for each line and appends them to an array
c = 0
while c < cols:
    r = 0
    while r < rows:
        linePosx = xs * (c - cols * 0.5 + xpos)
        pos[rows*c + r][0] = linePosx # Number of current row + number of elements in previous columns to get index
        linePosy = ys * (r - rows * 0.5 + ypos)
        pos[rows*c + r][1] = linePosy
        r = r + 1
    c = c + 1

# Creates a fading effect on the lines
# Computes distance from the center for each line and appends them to an array
c = 0
while c < cols:
    r = 0
    while r < rows:
        lineDistCenter = np.sqrt((pos[rows*c + r][0])**2 + (pos[rows*c + r][1])**2)
        distsCenter[rows*c + r] = lineDistCenter
        r = r + 1
    c = c + 1
# Assigns contrast to each line according to its distance from the center
c = 0
while c < cols:
    r = 0
    while r < rows:
        if distsCenter[rows*c + r] < radRing:
            lineContrast = 1
        else:
            lineContrast = 1 - lineContrDecrem * ((distsCenter[rows*c + r] - radRing)/6)
        matrixContrasts[rows*c + r] = lineContrast
        r = r + 1
    c = c + 1

# Creates grid
lines = visual.ElementArrayStim(win, elementTex = None, elementMask = None, fieldShape = 'sqr', fieldSize = 400, xys = pos, 
    sizes = lineSize, opacities = matrixContrasts, units = 'pix', nElements = nLines)

RandomOri = []
SquareOri = []
DiamondOri = []

# Builds list of orientations for Grid
# Generates orientations for random configuration and store in specific list
for randomConfig in range(90): # 50% * 60 trials + 60 baselines = 90 sets of random configurations
    # Defines auxiliary variables to keep track of segment orientations and set them to 0
    linesOri = np.ones(nLines) # creates array of ones to store orientations
    prevk = 0
    k = 0
    c = 0
    while c < cols:
        r = 0
        while r < rows:
            if r < (rows - 1):
                while (k == prevk) or (k == p[r]) or (k == prevDiag[r - 1]) or (k == p[r + 1]):
                    k = random.randint(1,12)
            else:
                while (k == prevk) or (k == p[r]) or (k == prevDiag[r - 1]):
                    k = random.randint(1,12)
            orientation = k * 15  # segment orientation ranges between 15 and 180 degrees; standard orientation is vertical (0 degrees)
            linesOri[rows*c + r] = orientation
            #if r > 0: # cannot update inferior previous diagonal if r = 0
            prevDiag[r - 1] = prevk #uses position of previous row to create a previous inferior diagonal for next column
            prevk = k #stores orientation of last row
            p[r] = k #stores orientation of last column
            r = r + 1
        c = c + 1
    RandomOri.append(linesOri)

# Generates orientations for square configuration and store in specific list
for squareConfig in range(24): #40% * 60 trials = 24 sets of square configurations
    linesOri = np.ones(nLines)
    prevk = 0
    k = 0
    c = 0
    while c < cols:
        r = 0
        while r < rows:
            if ((c == cols/2 - 6) or (c == cols/2 + 5)) and (r > rows/2 - 6) and (r < rows/2 + 5):
                k = 6
            elif ((r == rows/2 - 6) or (r == rows/2 + 5)) and (c > cols/2 - 6) and (c < cols/2 + 5):
                k = 12
            elif ((c == cols/2 - 6) and (r == rows/2 - 6)) or ((c == cols/2 + 5) and (r == rows/2 + 5)):
                k = 3
            elif ((c == cols/2 + 5) and (r == rows/2 - 6)) or ((c == cols/2 - 6) and (r == rows/2 + 5)):
                k = 9
            else:
                while (k == prevk) or (k == p[r]):
                    k = random.randint(1,12)
            orientation = k * 15
            linesOri[rows*c + r] = orientation
            prevk = k
            p[r] = k
            r = r + 1
        c = c + 1
    SquareOri.append(linesOri)

# Generates orientations for diamond configuration and store in specific list
for DiamondConfig in range(6): # 10% * 60 trials = 6 sets of diamond configurations
    linesOri = np.ones(nLines)
    prevk = 0
    k = 0
    c = 0
    while c < cols:
        r = 0
        while r < rows:
            if ((c == cols/2 - 7) and (r == rows/2)) or ((c == cols/2 - 6) and (r == rows/2 + 1)) or ((c == cols/2 - 5) and \
            (r == rows/2 + 2)) or ((c == cols/2 - 4) and (r == rows/2 + 3)) or ((c == cols/2 - 3) and (r == rows/2 + 4)) or \
            ((c == cols/2 - 2) and (r == rows/2 + 5)) or ((c == cols/2 - 1) and (r == rows/2 + 6)) or ((c == cols/2) and \
            (r == rows/2 - 7)) or ((c == cols/2 + 1) and (r == rows/2 - 6)) or ((c == cols/2 + 2) and (r == rows/2 - 5)) or \
            ((c == cols/2 + 3) and (r == rows/2 - 4)) or ((c == cols/2 + 4) and (r == rows/2 - 3)) or ((c == cols/2 + 5) and \
            (r == rows/2 - 2)) or ((c == cols/2 + 6) and (r == rows/2 - 1)):
                k = 9
            elif ((c == cols/2 - 7) and (r == rows/2 - 1)) or ((c == cols/2 - 6) and (r == rows/2 - 2)) or ((c == cols/2 - 5) and \
            (r == rows/2 - 3)) or ((c == cols/2 - 4) and (r == rows/2 - 4)) or ((c == cols/2 - 3) and (r == rows/2 - 5)) or \
            ((c == cols/2 - 2) and (r == rows/2 - 6)) or ((c == cols/2 - 1) and (r == rows/2 - 7)) or ((c == cols/2) and \
            (r == rows/2 + 6)) or ((c == cols/2 + 1) and (r == rows/2 + 5)) or ((c == cols/2 + 2) and (r == rows/2 + 4)) or \
            ((c == cols/2 + 3) and (r == rows/2 + 3)) or ((c == cols/2 + 4) and (r == rows/2 + 2)) or ((c == cols/2 + 5) and \
            (r == rows/2 + 1)) or ((c == cols/2 + 6) and (r == rows/2)):
                k = 3
            else:
                while (k == prevk) or (k == p[r]):
                    k = random.randint(1,12)
            orientation = k * 15
            linesOri[rows*c + r] = orientation
            prevk = k
            p[r] = k
            r = r + 1
        c = c + 1
    DiamondOri.append(linesOri)

# Create list with codes for the conditions
conditionList = [0 for i in range(30)] + [1 for j in range(24)] + [2 for k in range(6)]
conditionBlocks = []
for shuf in range(0, 10):
    random.shuffle(conditionList)
    tempList = [0] + conditionList[:]
    conditionBlocks.append(tempList)

# Baseline values for response-related variables
decrement = 'NaN'
RTs = []
RT = 'NaN'
Response = 0
correct = 'NaN'
stairStartVal = 0.75 # starting value for the first staircase
targetPosition = 99

# Create clocks
trialClock = core.Clock()
responseClock = core.Clock()
baseClock = core.Clock()

# Send markers for beggining of phase
if EEG == 1:
    if expInfo2['Phase number'] == '1':
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 16)
        time.sleep(0.002)
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
    if expInfo2['Phase number'] == '2':
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 32)
        time.sleep(0.002)
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
    if expInfo2['Phase number'] == '3':
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 48)
        time.sleep(0.002)
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)

# Present instructions
instructions = visual.TextStim(win, text = 'Fixeer je tijdens de taak op het rode kruis in het midden van het scherm.\n\n\
Eens elke seconde verschijnt er kort een ring met acht stippen op het scherm. \n\
In sommige gevallen zal een van de stippen iets donkerder zijn dan de overige stippen\n\n\
Druk op de spatiebalk wanneer een van de stippen donkerder is.\n\n\
Druk nu op de SPATIEBALK om met de taak te beginnen.\n\n\n\n\n\n\
Fixate on the red cross in the center of the display. \n \nOnce every second a ring with eight discs \
will briefly appear on the screen.\nOccasionally, one of the discs will be dimmer than the others. \n \nPress the button when you \
notice a dim disc\n\nPress SPACE to continue', pos = (0,0), height=24) 
instructions.draw()
win.flip()
keypress = event.waitKeys(keyList=['space', 'escape']) 
if keypress[0]=='escape':
    core.quit()
win.flip()
core.wait(1)


#---------Prepares Routine Baseline and Trial Sequence----------
for block in range(0,10):
    print(conditionBlocks[block])
    trial = 0
    staircase = data.QuestHandler(startVal = stairStartVal, startValSd = 0.3, stepType = 'lin', gamma = 0.0, pThreshold = 0.50, nTrials = numTrials + 1, minVal = 0,
            maxVal = 2) # defined inside outer loop because otherwise the outer loop skips it from block 2 on
    intensities = np.empty(numTrials)
    # Starts trial sequence
    if EEG == 1:
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 160)
        time.sleep(0.002)
        a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
    for thisIntensity in staircase:
        """
        In each iteration, trial, configuration and trial type (presence or absence of target) are only picked after a baseline and before a trial.
        Response related variables are reset at that point.
        
        Every block, a staircase is initiated. For staircases after the first, the last intensity from the previous block is used.
        
        For each trial, subjects may respond until the end of the following baseline. Feedback is presented either during baseline or during trials, 
        but not both. Data from the trial are written in the end of the trial in an iteration; data from the response, after the following baseline.
        
        After 60 iterations, a baseline is presented and the loop breaks, so as to provide the last trial with same window for response as
        previous trials.
        
        """
        # Defines baseline and trial durations
        baseConfig = random.uniform(0.484, 0.684)
        trialConfig = random.uniform(0.384, 0.484)
        # Presents grid in baseline configuration
        lines.oris = random.choice(RandomOri)
        lines.draw()
        fix1.draw()
        fix2.draw()
        win.flip()
        if trial > 0: # so as not to write down anything before participant's information has been written
            # Response-getting loop
            if expInfo2['Phase number'] == '3':
                if condition == 2:
                    baseConfig = baseConfig - feedbackTime
            else:
                if trialType == 0:
                    baseConfig = baseConfig - feedbackTime
            baseClock.reset()
            while baseClock.getTime() < baseConfig:
                keypress = event.getKeys(keyList = ['space', 'escape']) # avoids looping through baseConfig updating command by emptying response buffer
                if keypress:
                    responseGiven = 1
                    keypressTime = baseClock.getTime() # time elapsed until keypress to adjust baseConfig later
                    RT = responseClock.getTime()
                    if keypress[0] == 'space':
                        Response = 1
                        if expInfo2['Phase number'] == '3':
                            if condition == 2:
                                correct = 1    #correct
                                lines.draw()
                                fix1.draw()
                                fix2.draw()
                                feedback2.draw()
                                win.flip()
                                core.wait(feedbackTime)
                                staircase.addResponse(Response)  # adds response to stairhandler only for target-present trials
                                baseConfig = baseConfig - keypressTime
                            else:
                                correct = 0    #incorrect
                                lines.draw()
                                fix1.draw()
                                fix2.draw()
                                feedback1.draw()
                                win.flip()
                                core.wait(feedbackTime)
                                baseConfig = baseConfig - keypressTime - feedbackTime # computes remaining time for baseConfig after feedback presentation
                        else:
                            if trialType == 0:
                                correct = 1    #correct
                                lines.draw()
                                fix1.draw()
                                fix2.draw()
                                feedback2.draw()
                                win.flip()
                                core.wait(feedbackTime)
                                staircase.addResponse(Response)  # adds response to stairhandler only for target-present trials
                                baseConfig = baseConfig - keypressTime
                            else:
                                correct = 0    #incorrect
                                lines.draw()
                                fix1.draw()
                                fix2.draw()
                                feedback1.draw()
                                win.flip()
                                core.wait(feedbackTime)
                                baseConfig = baseConfig - keypressTime - feedbackTime # computes remaining time for baseConfig after feedback presentation
                    lines.draw()
                    fix1.draw()
                    fix2.draw()
                    win.flip()
                    if keypress[0] == 'escape':
                        core.quit()
            if responseGiven == 0:
                if expInfo2['Phase number'] == '3':
                    if condition == 2:
                        Response = 0
                        correct = 0    #incorrect
                        lines.draw()
                        fix1.draw()
                        fix2.draw()
                        feedback1.draw()
                        win.flip()
                        core.wait(feedbackTime)
                        staircase.addResponse(Response)  # adds response to stairhandler only for target-present trials
                    else:
                        Response = 0
                        correct = 1    #correct
                else:
                    if trialType == 0:
                        Response = 0
                        correct = 0    #incorrect
                        lines.draw()
                        fix1.draw()
                        fix2.draw()
                        feedback1.draw()
                        win.flip()
                        core.wait(feedbackTime)
                        staircase.addResponse(Response)  # adds response to stairhandler only for target-present trials
                    else:
                        Response = 0
                        correct = 1    #correct
            f.write(str(RT) + "\t" + str(Response) + "\t" + str(correct)) #writes RT and responses from previous trials
            f.write("\n")
        keypress = event.clearEvents(eventType = None)
        decrement = 'NaN'
        if trial == numTrials:
            break
        # Assigns orientation for trial
        if expInfo2['Phase number'] == '0':
            condition = 0
            configuration = 0 #random
        elif expInfo2['Phase number'] == '1' or expInfo2['Phase number'] == '2' or expInfo2['Phase number'] == '3':
            # Defines condition of trial
            condition = conditionBlocks[block][trial]
        if condition == 0:
            lines.oris = random.choice(RandomOri)
            configuration = 0 #random
        elif condition == 1:
            lines.oris = random.choice(SquareOri)
            configuration = 1 #square
        else:
            lines.oris = random.choice(DiamondOri)
            configuration = 2 #diamond
        # Presents trial grid
        lines.draw()
        # Determine if a target will be presented
        targetPosition = 99
        trialType = random.randint(0,9)
        if trialType == 0:
            targetPresence = 1
            targetPosition = random.randint(0,7) # chooses target randomly among the discs
            decrement = thisIntensity
        else:
            targetPresence = 0
        # Target presentation
        # Returns discs to base color in order to reset previous target
        disc0.color = discBaseColor
        disc1.color = discBaseColor
        disc2.color = discBaseColor
        disc3.color = discBaseColor
        disc4.color = discBaseColor
        disc5.color = discBaseColor
        disc6.color = discBaseColor
        disc7.color = discBaseColor
        # Dims target
        if trialType == 0:
            redValue = 1 - thisIntensity
            if targetPosition == 0:
                disc0.color = (redValue, -1, -1)
            elif targetPosition == 1:
                disc1.color = (redValue, -1, -1)
            elif targetPosition == 2:
                disc2.color = (redValue, -1, -1)
            elif targetPosition == 3:
                disc3.color = (redValue, -1, -1)
            elif targetPosition == 4:
                disc4.color = (redValue, -1, -1)
            elif targetPosition == 5:
                disc5.color = (redValue, -1, -1)
            elif targetPosition == 6:
                disc6.color = (redValue, -1, -1)
            elif targetPosition == 7:
                disc7.color = (redValue, -1, -1)
        ring.draw()
        disc0.draw()
        disc1.draw()
        disc2.draw()
        disc3.draw()
        disc4.draw()
        disc5.draw()
        disc6.draw()
        disc7.draw()
        fix1.draw()
        fix2.draw()
        win.flip()
        if EEG == 1:
            if condition == 0 and targetPresence == 1:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 64)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
            elif condition == 0 and targetPresence == 0:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 80)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
            elif condition == 1 and targetPresence == 1:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 96)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
            elif condition == 1 and targetPresence == 0:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 112)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
            elif condition == 2 and targetPresence == 1:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 128)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
            elif condition == 2 and targetPresence == 0:
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 144)
                time.sleep(0.002)
                a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
        Response = 0
        RT = 'NaN' # Reset response variables to baseline values for target-absent trials
        correct = 'NaN'
        trialClock.reset()
        responseClock.reset()
        responseGiven = 0 # variable to keep track of response status and avoid repetition of feedback in trial and baseline
        while trialClock.getTime() < trialConfig:
        # Get Responses
            keypress = event.getKeys(keyList = ['space', 'escape']) # avoids looping through baseConfig updating by emptying response buffer
            if keypress:
                responseGiven = 1
                keypressTime = trialClock.getTime()
                RT = responseClock.getTime()
                if keypress[0] == 'escape':
                    core.quit()
                elif keypress[0] == 'space':
                    Response = 1
                    if expInfo2['Phase number'] == '3':
                        if condition == 2:
                            correct = 1    #correct
                            lines.draw()
                            ring.draw()
                            disc0.draw()
                            disc1.draw()
                            disc2.draw()
                            disc3.draw()
                            disc4.draw()
                            disc5.draw()
                            disc6.draw()
                            disc7.draw()
                            fix1.draw()
                            fix2.draw()
                            feedback2.draw()
                            win.flip()
                            core.wait(feedbackTime)
                            trialConfig = trialConfig - keypressTime - feedbackTime
                        else:
                            correct = 0    #incorrect
                            lines.draw()
                            ring.draw()
                            disc0.draw()
                            disc1.draw()
                            disc2.draw()
                            disc3.draw()
                            disc4.draw()
                            disc5.draw()
                            disc6.draw()
                            disc7.draw()
                            fix1.draw()
                            fix2.draw()
                            feedback1.draw()
                            win.flip()
                            core.wait(feedbackTime)
                            trialConfig = trialConfig - keypressTime - feedbackTime
                    else:
                        if trialType == 0:
                            correct = 1    #correct
                            lines.draw()
                            ring.draw()
                            disc0.draw()
                            disc1.draw()
                            disc2.draw()
                            disc3.draw()
                            disc4.draw()
                            disc5.draw()
                            disc6.draw()
                            disc7.draw()
                            fix1.draw()
                            fix2.draw()
                            feedback2.draw()
                            win.flip()
                            core.wait(feedbackTime)
                            staircase.addResponse(Response)  # adds response to stairhandler only for target-present trials
                            trialConfig = trialConfig - keypressTime - feedbackTime
                        else:
                            correct = 0    #incorrect
                            lines.draw()
                            ring.draw()
                            disc0.draw()
                            disc1.draw()
                            disc2.draw()
                            disc3.draw()
                            disc4.draw()
                            disc5.draw()
                            disc6.draw()
                            disc7.draw()
                            fix1.draw()
                            fix2.draw()
                            feedback1.draw()
                            win.flip()
                            core.wait(feedbackTime)
                            trialConfig = trialConfig - keypressTime - feedbackTime
                lines.draw()
                ring.draw()
                disc0.draw()
                disc1.draw()
                disc2.draw()
                disc3.draw()
                disc4.draw()
                disc5.draw()
                disc6.draw()
                disc7.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
        if EEG == 1:
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 208)
            time.sleep(0.002)
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
        intensities[trial] = thisIntensity
        trial += 1
        f.write(expInfo['Participant number'] + "\t" + expInfo2['Phase number'] + "\t" + str(block) + "\t" + str(trial) + "\t" + str(targetPresence) + "\t" + str(configuration) + \
        "\t" + str(decrement) + "\t")
        print(trial)
    print(intensities)
    print('END OF BLOCK ' + str(block))
    stairStartVal = intensities[-1]
    if expInfo2['Phase number'] == '0' and block == 4:
        break
    if block < 9:
        breakText.draw()
        win.flip()
        if EEG == 1:
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 224)
            time.sleep(0.002)
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
        event.waitKeys(keyList=['space'])
        keypress = event.clearEvents(eventType = None) # to clear any accidental presses during rests
    else:
        print('-------------END OF PHASE ' + str(expInfo2['Phase number']) + '!!!-------------')
        endPhaseText.draw()
        win.flip()
        if EEG == 1:
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 240)
            time.sleep(0.002)
            a =PCIDAQ24.cbDOut(BoardNum,PortNum, 0)
        event.waitKeys(keyList=['space'])
        if expInfo2['Phase number'] == '1' or expInfo2['Phase number'] == '2':
            # ------- Start patterns presentation----------
            for question in range(2):
                # Diamond pattern
                diamondOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((c == cols/2 - 7) and (r == rows/2)) or ((c == cols/2 - 6) and (r == rows/2 + 1)) or ((c == cols/2 - 5) and \
                        (r == rows/2 + 2)) or ((c == cols/2 - 4) and (r == rows/2 + 3)) or ((c == cols/2 - 3) and (r == rows/2 + 4)) or \
                        ((c == cols/2 - 2) and (r == rows/2 + 5)) or ((c == cols/2 - 1) and (r == rows/2 + 6)) or ((c == cols/2) and \
                        (r == rows/2 - 7)) or ((c == cols/2 + 1) and (r == rows/2 - 6)) or ((c == cols/2 + 2) and (r == rows/2 - 5)) or \
                        ((c == cols/2 + 3) and (r == rows/2 - 4)) or ((c == cols/2 + 4) and (r == rows/2 - 3)) or ((c == cols/2 + 5) and \
                        (r == rows/2 - 2)) or ((c == cols/2 + 6) and (r == rows/2 - 1)):
                            k = 9
                        elif ((c == cols/2 - 7) and (r == rows/2 - 1)) or ((c == cols/2 - 6) and (r == rows/2 - 2)) or ((c == cols/2 - 5) and \
                        (r == rows/2 - 3)) or ((c == cols/2 - 4) and (r == rows/2 - 4)) or ((c == cols/2 - 3) and (r == rows/2 - 5)) or \
                        ((c == cols/2 - 2) and (r == rows/2 - 6)) or ((c == cols/2 - 1) and (r == rows/2 - 7)) or ((c == cols/2) and \
                        (r == rows/2 + 6)) or ((c == cols/2 + 1) and (r == rows/2 + 5)) or ((c == cols/2 + 2) and (r == rows/2 + 4)) or \
                        ((c == cols/2 + 3) and (r == rows/2 + 3)) or ((c == cols/2 + 4) and (r == rows/2 + 2)) or ((c == cols/2 + 5) and \
                        (r == rows/2 + 1)) or ((c == cols/2 + 6) and (r == rows/2)):
                            k = 3
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        diamondOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = diamondOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
                # Horizontal rectangle
                horizRectOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((c == cols/2 - 8) or (c == cols/2 + 7)) and (r > rows/2 - 4) and (r < rows/2 + 3):
                            k = 6
                        elif ((r == rows/2 - 4) or (r == rows/2 + 3)) and (c > cols/2 - 9) and (c < cols/2 + 8):
                            k = 12
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        horizRectOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = horizRectOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
                # X pattern
                xPatternOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((c > cols/2 - 10) and (c < cols/2 - 1) and (r > rows/2 - 10) and (r < rows/2 - 1) and (c == r + 10)):
                            k = 9
                        elif ((c > cols/2) and (c < cols/2 + 9) and (r > rows/2) and (r < rows/2 + 9) and (c == r + 10)):
                            k = 9
                        elif ((c == cols/2 - 9) and (r == rows/2 + 8)) or ((c == cols/2 - 8) and (r == rows/2 + 7)) or ((c == cols/2 - 7) and \
                        (r == rows/2 + 6)) or ((c == cols/2 - 6) and (r == rows/2 + 5)) or ((c == cols/2 -5) and (r == rows/2 + 4)) or ((c == cols/2 - 4) and \
                        (r == rows/2 + 3)) or ((c == cols/2 - 3) and (r == rows/2 + 2)) or (( c == cols/2 - 2) and (r == rows/2 + 1)) or \
                        (( r == rows/2 - 9) and (c == cols/2 + 8)) or ((r == rows/2 - 8) and (c == cols/2 + 7)) or ((r == rows/2 - 7) and \
                        (c == cols/2 + 6)) or ((r == rows/2 - 6) and (c == cols/2 + 5)) or ((r == rows/2 - 5) and (c == cols/2 + 4)) or ((r == rows/2 - 4) and \
                        (c == cols/2 + 3)) or ((r == rows/2 - 3) and (c == cols/2 + 2)) or ((r == rows/2 - 2) and (c == cols/2 + 1)):
                            k = 3
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        xPatternOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = xPatternOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
                # 1 big Square
                SquareOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((c == cols/2 - 6) or (c == cols/2 + 5)) and (r > rows/2 - 6) and (r < rows/2 + 5):
                            k = 6
                        elif ((r == rows/2 - 6) or (r == rows/2 + 5)) and (c > cols/2 - 6) and (c < cols/2 + 5):
                            k = 12
                        elif ((c == cols/2 - 6) and (r == rows/2 - 6)) or ((c == cols/2 + 5) and (r == rows/2 + 5)):
                            k = 3
                        elif ((c == cols/2 + 5) and (r == rows/2 - 6)) or ((c == cols/2 - 6) and (r == rows/2 + 5)):
                            k = 9
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        SquareOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = SquareOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
                # 4 squares
                fourSquareOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((c == cols/2 - 9) or (c == cols/2 + 2)) and (((r > rows/2 -10) and (r < rows/2 - 3)) or ((r > rows/2 + 1) and (r < rows/2  + 8))):
                            k = 6
                        elif ((c == cols/2 - 3) or (c == cols/2 + 8)) and (((r > rows/2 - 9) and (r < rows/2 - 2)) or ((r > rows/2 + 2) and (r < rows/2 + 9))):
                            k = 6
                        elif ((r == rows/2 - 9) or (r == rows/2 + 2)) and (((c > cols/2 - 9) and (c < cols/2 - 2)) or ((c > cols/2 + 2) and (c < cols/2 + 9))):
                            k = 12
                        elif ((r == rows/2 - 3) or (r == rows/2 + 8)) and (((c > cols/2 - 10) and (c < cols/2 - 3)) or ((c > cols/2 + 1) and (c < cols/2 + 8))):
                            k = 12
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        fourSquareOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = fourSquareOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
                # Vertical rectangle
                vertRectOri = np.ones(nLines)
                prevk = 0
                k = 0
                c = 0
                while c < cols:
                    r = 0
                    while r < rows:
                        if ((r == rows/2 - 8) or (r == rows/2 + 7)) and (c > cols/2 - 4) and (c < cols/2 + 3):
                            k = 12
                        elif ((c == cols/ 2 - 4) or (c == cols/2 + 3)) and (r > rows/2 - 9) and (r < rows/2 + 8):
                            k = 6
                        else:
                            while (k == prevk) or (k == p[r]):
                                k = random.randint(1,12)
                        orientation = k * 15
                        vertRectOri[rows*c + r] = orientation
                        prevk = k
                        p[r] = k
                        r = r + 1
                    c = c + 1
                lines.oris = vertRectOri
                lines.draw()
                fix1.draw()
                fix2.draw()
                win.flip()
                event.waitKeys('space')
win.close()
core.quit()