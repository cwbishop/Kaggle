import cv2
import numpy as np
from matplotlib import pyplot as plt

# Use this as matplotlib backend Qt4Agg
train = 'train/14.png'
train_clean = 'train_cleaned/14.png'

class OCRImage:


	def __init__(self, data):
		""" can be initialized as either a filename or an image """

		if isinstance(data, basestring):
			""" Read in the image and store the file information """
			self.image = self.read_image(data)
			self.file_name = data

		else:
			self.image = data
			self.file_name = "" # can make this smarter

		# Save original image dimensions
		self.orig_shape = self.get_shape()

	def read_image(self, file_name):
		""" Read in an image """
		return(cv2.imread(file_name, 0))

	#################
	# Attribute grabs
	#################
	def get_shape(self):
		return(self.image.shape)

	#################
	# Data processing
	#################
	def canny_edge(self, min_val, max_val):
		""" Returns an image object with canny edges """
		canny_image = cv2.Canny(self.image, min_val, max_val)

		# Create a new OCRImage object for further processing
		return(OCRImage(canny_image))

	def gaussian_blur(self, width_height, std):
		""" Gaussian blur image """
		blur_image = cv2.GaussianBlur(self.image, width_height, std)
		self.image = blur_image

		return(OCRImage(blur_image))

	def fft2(self):
		"""Computes the 2D FFT of the image

		See here for example of plotting the magnitude spectrum
		http://stackoverflow.com/questions/9413216/simple-digit-recognition-ocr-in-opencv-python
		"""		
		# 2D FFT
		# return(OCRImage(np.fft.fft2(np.float32(self.image))))
		self.image = np.fft.fft2(np.float32(self.image))
		# return(OCRImage(np.fft.fft2(np.float32(self.image))))
	
	#################
	# Visualization
	#################
	def plot_image(self):
		""" Creates a plot """
		axes = plt.imshow(self.image, 'gray')
		plt.title(self.file_name)
		plt.xticks([])
		plt.yticks([])
		return(axes)
		# plt.show()

	def plot_mag(self):
		"""
		Plots the magnitude specturm of the image
		http://docs.opencv.org/3.0-beta/doc/py_tutorials/py_imgproc/py_transforms/py_fourier_transform/py_fourier_transform.html
		"""

		# Get FFT
		fft = self.fft2().image

		# Shift so 0 is in center
		fft_shift = np.fft.fftshift(fft)

		# Convert to decibels
		magnitude_spectrum = 20*np.log(np.abs(fft_shift))

		# Assign magnitude spectrum
		self.image = magnitude_spectrum

		# Generate plot
		self.plot_image()

		# Add colorbar
		plt.colorbar()

# Get list of training images
# class TrainPair:
# 	""" Loads clean/dirty images """

# 	def __init__(self, file_name):
# 		""" file_name: 14.png (no path information) """
		
# 		# Load clean and dirty images		
# 		self.clean = OCRImage('train_cleaned/' + file_name)
# 		self.dirty = OCRImage('train/' + file_name)
# 		self.orig_shape = self.clean.get_shape()

# 	def plot_image(self, figure_size=(10,10)):
# 		""" 
# 		plots the image pair 

# 		plot clean image on top, dirty image below
# 		"""
# 		plt.figure(figsize=figure_size)
# 		plt.subplot(2,1,1)
# 		self.clean.plot_image()
# 		plt.subplot(2,1,2)
# 		self.dirty.plot_image()
# 		plt.show()


class ImageList:
	""" Arbitrary list of OCRImage objects """
	def __init__(self):
		self.image = list()		

	def append(self, data):
		""" Appends an image file for the indicated file """
		self.image.append(OCRImage(data))

	def plot_image(self, figure_size=(10, 10)):
		""" 
		Creates a subplot of images 

		Proved useful when plotting multiple images at once
		"""

		# Create the figure with specified dimensions
		plt.figure(figsize=figure_size)

		for i in range(len(self.image)):
			plt.subplot(len(self.image),1,i+1)
			self.image[i].plot_image()

	def fft2(self):
		""" Compute fft2 for list of images """
		for i in range(len(self.image)):
			self.image[i]

	
	def plot_mag(self, figure_size=(10, 10)):
		"""
		Plot magnitude specturm for all image
		"""

		fig = plt.figure(figsize=figure_size)

		# Transform all images
		for i in range(len(self.image)):
			
			plt.subplot(len(self.image),1,i+1)
			
			self.image[i].plot_mag()

		# Apply an approach
#	- Canny edge detection
#	- Smoothing + threshold
#	- 