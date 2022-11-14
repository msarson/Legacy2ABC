## Legacy and ABC

Clarion published a PDF discussing transition from Legacy to ABC. 
 Chapter 3 is the only documentation on the ABC Conversion Tools.

[Learn ABC Transition and Conversion](LearnABC_Transition_Conversion.pdf)

In 2000 Simon Brewer published "Legacy to ABC: There is Another Way!" in Clarion Magazine
 on his method of converting Legacy to ABC. This article series is based on a
 presentation Simon gave at ConVic 1999.

> Daunted by the challenge of migrating your apps from Legacy to ABC? Simon Brewer 
> shows how to do it one piece at a time with a hybrid of Legacy and ABC code. 

[Legacy to ABC - There is Another Way, Part 1](Cmag-2000-07_LegacytoABC-ThereIsAnotherWay.pdf)
[Legacy to ABC - There is Another Way, Part 2 and 3](Cmag-2000-08_LegacytoABC-ThereIsAnotherWay.pdf)

Conversion should not be viewed as an All or Nothing process.
 His technique is a way to divide and conquer doing the process in small parts.
 This fits with my way of creating an ABC Data DLL that can also be shared by Legacy.
 It would also work with my the simpler first try that had separate Legacy and ABC Data DLLs.
 Parts can remain Legacy while parts are converted to ABC. Or only new work can be done in ABC.

Simon converts the CW 2.0 Club Manager in Legacy. I'll upload that in this Repo.

### ClubMgrLegacy.zip

Included in this folder is ClubMgrLegacy.zip containing the CW 2.0  Legacy example APP.
 I opened it in Clarion 10 and fixed all the problems so it builds clean. The .6App and DCT are still there if you want the original.
 You could use this APP to try the conversion as Simnon did in his articles.

ClubMgrLegacy.zip is in the TestAPP folder