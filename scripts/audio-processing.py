import numpy as np
import matplotlib.pyplot as plt
from scipy.io import wavfile
from scipy.fft import fft, fftfreq

samplerate, data = wavfile.read("D:\\26 WAYNE\\3.1\\Stats\\Project\\roar-katyperry.wav")

# If stereo, take only one channel
if len(data.shape) > 1:
    data = data[:, 0]

fft_data = fft(data)
frequencies = fftfreq(len(data), 1 / samplerate)

'''
plt.plot(frequencies, np.abs(fft_data))
plt.xlabel('Frequency (Hz)')
plt.ylabel('Amplitude')
plt.title('FFT of Audio Signal')
plt.show()
'''
