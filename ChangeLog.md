# Revision history for mmsyn6ukr

## 0.1.0.0 -- 2019-11-25

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2019-11-25

* Second version. Added the possibility for program to use the installed
SoX binary in the system to automatically create a raw PCM file into
the .wav sound file that can be easily listened to.

## 0.2.0.1 -- 2019-11-25

* Second version revised A. Some minor code changes.

## 0.3.0.0 -- 2019-11-28

* Third version. Fixed an issue with the unnecessary additional file.
Changed the package into executable and library format.

## 0.3.0.1 -- 2019-11-28

* Third version revised A. Fixed an issue with the bounds for the library in the .cabal file.

## 0.3.1.0 -- 2019-11-29

* Third version revised B. Changed some sounds or sound representation: "и", "й", "в", "ь", "ць".

## 0.3.2.0 -- 2019-11-29

* Third version revised C. Changed some sounds or sound representation: "с", "сь", "х", "ч", "ш".

## 0.3.3.0 -- 2019-12-03

* Third version revised D. Changed all non-silent sounds. Added support for '-'.

## 0.3.4.0 -- 2019-12-03

* Third version revised E. Fixed issues with the jotted sounds.

## 0.4.0.0 -- 2019-12-05

* Fourth version. Fixed issues with the sound conversions. Now they works properly.

## 0.4.0.1 -- 2019-12-06

* Fourth version revised A. Better documented. Some minor code changes.

## 0.4.0.2 -- 2019-12-06

* Fourth version revised B. Fixed issues with the documentation.

## 0.4.0.3 -- 2019-12-06

* Fourth version revised C. Fixed issues with the documentation.

## 0.4.0.4 -- 2019-12-06

* Fourth version revised D. Fixed issues with the documentation. Fixed an issue with the nSymbols function in the Melodics.Ukrainian module.

## 0.4.0.5 -- 2019-12-06

* Fourth version revised E. Improved the documentation.

## 0.4.0.6 -- 2019-12-06

* Fourth version revised F. Fixed an issue with the documentation for Main.main function.

## 0.4.1.0 -- 2019-12-06

* Fourth version revised G. Improved the documentation for the Main.main function. Added the possibility to automatically delete the unneeded .raw file.

## 0.4.2.0 -- 2019-12-06

* Fourth version revised H. The program if there is SoX installed in the system converts the .raw file to .wav file and deletes the first one to save space.

## 0.4.2.1 -- 2019-12-06

* Fourth version revised I. Minor improvements to documentation.

## 0.4.2.2 -- 2019-12-06

* Fourth version revised J. Added an informative message after the creation of the .raw file.

## 0.5.0.0 -- 2019-12-07

* Fifth version. Changed a behaviour of the Main.main function, added a possibility of more control on the program mmsyn6ukr.

## 0.5.1.0 -- 2019-12-09

* Fifth version revised A. Fixed the genControl function, added more documentation, make the exception message more exact.

## 0.5.2.0 -- 2019-12-16

* Fifth version revised B. Added support for better pauses and therefore additional file "-.wav". Fixed issues with the conversion in case of punctuation separated words.
Added additional information about sound files. Deprecated (===) operator in favour of `elem` function.

## 0.6.0.0 -- 2019-12-18

* Sixth version. Changed the module structure.

## 0.6.1.0 -- 2019-12-24

* Sixth version revised A. Changed the dependencies bounds so that it can now be compiled for the GHC 8.8.1.

## 0.6.2.0 -- 2019-12-24

* Sixth version revised B. Changed the imported functions in the Melodics.Ukrainian module.

## 0.6.3.0 -- 2020-01-31

* Sixth version revised C. Changed a README.md file to README.markdown. Some documentation improvements.

## 0.6.3.1 -- 2020-02-23

* Sixth version revised D. Fixed an issue with word usage (a terminology) in the documentation for 'genControl' function.

## 0.6.4.0 -- 2020-05-11

* Sixth version revised E. Changed sound representations for the Ukrainian "и" -- file "K.wav", "к" -- file "M.wav", "т" -- file "V.wav", "ч" -- file "b.wav",
"і" -- file "e.wav".

## 0.6.5.0 -- 2020-05-12

* Sixth version revised F. Changed sound representations for the Ukrainian "б" -- file "B.wav", "д" -- file "E.wav", "дж" -- file "F.wav",
"ж" -- file "I.wav", "п" -- file "R.wav", "р" -- file "S.wav". Changed durations accordingly in the Map.txt file.

## 0.6.6.0 -- 2020-05-12

* Sixth version revised G. Changed sound representations for the Ukrainian "б" -- file "B.wav", "к" -- file "M.wav".

## 0.7.0.0 -- 2020-05-12

* Seventh version. Changed the module structure (and some functions accordinly, too) to more logical and convenient for factorization and composition.
Added a new module Melodics.Executable with the most of the Main module processment functions. Changed the behaviour of the executable
into repeated so you can create many files with it during one runtime operation.

## 0.7.1.0 -- 2020-05-12

* Seventh version revised A. Fixed issues with being not circled because of the semi-closed handles.

## 0.7.2.0 -- 2020-05-14

* Seventh version revised B. Fixed bounds of the dependencies so that now it is supported also for GHC 8.10* series.

## 0.7.3.0 -- 2020-06-01

* Seventh version revised C. Added Melodics.Executable.recFileName function to the export list of the module because it is re-used by the dobutokO-poetry
package.

## 0.8.0.0 -- 2020-08-16

* Eighth version. Added a new module MMSyn6Ukr.Show7s with the function from mmsyn7s module. It is done for the possible refactoring and changing the dependencies
for other packages (first of all, mmsyn7h).

## 0.8.1.0 -- 2020-09-25

* Eighth version revised A. Added to the exported functions isUkrainian from the Melodics.Ukrainian module.

## 0.8.2.0 -- 2020-10-28

* Eighth version revised B. Fixed issues with some combinations that were not
properly handled.

## 0.8.3.0 -- 2020-10-29

* Eighth version revised C. Changed the dependencies boundaries. Some minor code improvements.

## 0.9.0.0 -- 2020-11-07

* Ninth version. Switched to the ukrainian-phonetics-basic package (and so added it as a new dependency) as the engine of the phonetics conversions and
representations. By this fixed some issues related to complex letter combinations. Please, update all the dependent packages related to the mmsyn6ukr
to use the new approach.

