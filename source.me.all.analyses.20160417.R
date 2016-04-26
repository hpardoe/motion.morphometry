# All analyses in "Motion and morphometry in clinical and nonclinical populations" except for vertex-wise or voxel-wise analyses
load("pardoe.motion.morphometry.20160416.RData")

# motion.df is less unwieldy than the loaded name
motion.df <- pardoe.motion.morphometry.20160416.df
rm(pardoe.motion.morphometry.20160416.df)

# Abstract, results
# number of participants
dim(motion.df)

# cortical thickness across all studies
summary(lm(freesurfer.mean.thickness ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# cortical contrast across all studies
summary(lm(freesurfer.mean.contrast ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# rsfMRI-derived motion and qualitative ratings
summary(lm(mean.rms ~ motion.artifact, data = motion.df))

# ABIDE relationship between motion and demographic variables
summary(lm(mean.rms ~ diagnosis + age + gender + site, data = motion.df[ motion.df$study == "abide", ]))

# ADHD-200 relationship between motion and demographic variables
summary(lm(mean.rms ~ diagnosis + age + gender + site, data = motion.df[ motion.df$study == "adhd", ]))

# COBRE relationship between motion and demographic variables
summary(lm(mean.rms ~ diagnosis + age + gender, data = motion.df[ motion.df$study == "cobre", ]))

# Freesurfer mean cortical thickness and motion
summary(lm(freesurfer.mean.thickness ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# ANTs mean cortical thickness and motion
summary(lm(ants.thickness.mm ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# Freesurfer contrast and motion
summary(lm(freesurfer.mean.contrast ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# Freesurfer supratentorial volume and motion
summary(lm(freesurfer.SupraTentorialVol ~ mean.rms + diagnosis + gender + age + site + freesurfer.EstimatedTotalIntraCranialVol, data = motion.df[ motion.df$motion.artifact == 5, ]))

# Freesurfer TIV and motion
summary(lm(freesurfer.EstimatedTotalIntraCranialVol ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$motion.artifact == 5, ]))

# FSL Brain Volume
# high quality
summary(lm(fsl.bvol.mm ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ (motion.df$fsl.qa.pass) &(motion.df$motion.artifact == 5), ]))
# all scans
summary(lm(fsl.bvol.mm ~ mean.rms + diagnosis + gender + age + site, data = motion.df[ motion.df$fsl.qa.pass, ]))

# FSL GM volume
# high quality
summary(lm(fsl.gmvol.mm ~ mean.rms + diagnosis + gender + age + site + fsl.bvol.mm, data = motion.df[ (motion.df$fsl.qa.pass) & (motion.df$motion.artifact == 5), ]))
# all scans
summary(lm(fsl.gmvol.mm ~ mean.rms + diagnosis + gender + age + site + fsl.bvol.mm, data = motion.df[ (motion.df$fsl.qa.pass) & (motion.df$motion.artifact == 5), ]))

# FSL WM volume
# high quality
summary(lm(fsl.wmvol.mm ~ mean.rms + diagnosis + gender + age + site + fsl.bvol.mm, data = motion.df[ (motion.df$fsl.qa.pass) & (motion.df$motion.artifact == 5), ]))
# all scans
summary(lm(fsl.wmvol.mm ~ mean.rms + diagnosis + gender + age + site + fsl.bvol.mm, data = motion.df[ (motion.df$fsl.qa.pass) & (motion.df$motion.artifact == 5), ]))

# Freesurfer volumetric analyses
struct.name.vector <- c()
beta.qa.val.vector <- c()
p.qa.val.vector <- c()
beta.no.qa.val.vector <- c()
p.no.qa.val.vector <- c()

for (i in 16:80) {
struct.name <- names(motion.df)[i]

temp.qa <- summary(lm(motion.df[motion.df$motion.artifact == 5,i] ~ mean.rms + diagnosis + gender + age + site + freesurfer.EstimatedTotalIntraCranialVol, data = motion.df[ motion.df$motion.artifact == 5, ]))
beta.qa.val <- temp.qa$coefficients[2,1]
p.qa.val <- temp.qa$coefficients[2,4]

temp.no.qa <- summary(lm(motion.df[,i] ~ mean.rms + diagnosis + gender + age + site + freesurfer.EstimatedTotalIntraCranialVol, data = motion.df))
beta.no.qa.val <- temp.no.qa$coefficients[2,1]
p.no.qa.val <- temp.no.qa$coefficients[2,4]

struct.name.vector <- c(struct.name.vector,struct.name)
beta.qa.val.vector <- c(beta.qa.val.vector, beta.qa.val)
p.qa.val.vector <- c(p.qa.val.vector, p.qa.val)
beta.no.qa.val.vector <- c(beta.no.qa.val.vector,beta.no.qa.val)
p.no.qa.val.vector <- c(p.no.qa.val.vector,p.no.qa.val)
}

freesurfer.vol.summary <- data.frame(structure = struct.name.vector, effect.size.qa = beta.qa.val.vector, p.val.qa = p.qa.val.vector, effect.size.no.qa = beta.no.qa.val.vector, p.val.no.qa = p.no.qa.val.vector)