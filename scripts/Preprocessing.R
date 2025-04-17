setwd("D:\\26 WAYNE\\3.2\\Data Science\\Project\\RawAcapellaData\\csv\\dirty")
remove(list=ls())

# 1. Load data and remove items outside of singer's range
data = read.csv("sorry-justinbieber.f0.csv")
plot(data$frequency ~ data$time, lty=1)

data = data[data$frequency < 650,] #upper limit
data = data[data$frequency > 100,] #lower limit

data2 = data[1,] #clean version of data for smoothing

# 2. Remove octaves (twice or half of previous data point)
for(i in 2:nrow(data)){
  prev = data2$frequency[nrow(data2)]
  curr = data$frequency[i]
  prev
  curr
  if (curr >= prev / 2 && curr <= prev * 2) {
    data2 = rbind(data2, data[i,])
  }
}

# 3. Add a row with the note value
freq2note = function(freq){
  notes = c("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
  
  note_num = 12 * log(freq / 440, 2) + 49
  note_num = round(note_num)
  
  note_index = ((note_num - 1 ) %% length(notes)) + 1
  note_name = notes[note_index]
  
  octave = (note_num + 8 ) %/% length(notes)
  
  return(note_name)
}
rnote = sapply(data2$frequency, FUN=freq2note)
data2 = cbind(data2, note=rnote)


plot(data2$frequency ~ data2$time, lty=1)


# 4. Finally, I want 32nd note data
# a) Make a matrix where each row is the duration of a 32nd note
#1000ms | 60 s | 1 min    | 1 beat
#1 s    | 1 min| 90 beats | 8 eighths of a beat
#Final unit: s / eighths of a beat
song_tempo = 100
bin_width = 60 / song_tempo / 8
bin_width
#divide into bins of that width
bins = cut(data2$time, breaks = seq(min(data2$time), max(data2$time) + bin_width, by = bin_width), include.lowest = TRUE)
unique_bins = levels(bins)
#vector for frequencies and notes
freqs = numeric(length(unique_bins))
notes = character(length(unique_bins))
#auxilary function to compute mode
mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
#actual calculation
for (i in seq_along(unique_bins)) {
  bin_indices = which(bins == unique_bins[i])
  
  this_freqs = data2$frequency[bin_indices]
  this_notes = data2$note[bin_indices]
  
  if(length(this_freqs) > 0){
    freqs[i] = mean(this_freqs, na.rm = TRUE)
  }else{
    freqs[i] = NA
  }
  
  if(length(this_notes) > 0){
    notes[i] = mode(this_notes)
  }else{
    freqs[i] = NA
  }
}
data3 = data.frame(bin = unique_bins, frequency = freqs, note = notes)
plot(data3$frequency, lty=1)

#export!
write.csv(data3, "sorry-justinbieber-clean.csv")
