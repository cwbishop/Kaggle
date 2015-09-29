import cv2
import numpy as np
from matplotlib import pyplot as plt
import os # to list files ina  directory
from os import path
import pandas as pd

# Use this as matplotlib backend Qt4Agg
# train = 'train/14.png'
# train_clean = 'train_cleaned/14.png'

train_dirty = os.listdir('train')
train_dirty = ['train/{0}'.format(i) for i in train_dirty]
train_dirty = [f for f in train_dirty if path.isfile(f)]
train_clean = os.listdir('train_cleaned')
train_clean = ['train_cleaned/{0}'.format(i) for i in train_clean]
train_clean = [f for f in train_clean if path.isfile(f)]
test = os.listdir('test')
test = ['test/{0}'.format(i) for i in test]
test_clean = [f for f in test if path.isfile(f)]

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

	def get_fftfreq(self):
		""" 
		Returns the (row, column) frequency bins for FFT.
		Frequencies return in cycles/pixel

		Note: this should only be called after FFT2 has been run
		"""

		return( 
				(np.fft.fftfreq(self.get_shape()[0], d=1),
				np.fft.fftfreq(self.get_shape()[1], d=1))
			)
		
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
		# self.image = blur_image

		return(OCRImage(blur_image))

	def fft2(self):
		"""Computes the 2D FFT of the image

		See here for example of plotting the magnitude spectrum
		http://stackoverflow.com/questions/9413216/simple-digit-recognition-ocr-in-opencv-python
		"""		
		# 2D FFT
		# return(OCRImage(np.fft.fft2(np.float32(self.image))))
		# self.image = np.fft.fft2(np.float32(self.image))
		return(OCRImage(np.fft.fft2(np.float32(self.image))))

	def mean(self, axis):
		"""
		Returns mean across specified axis
		"""
		return(OCRImage(self.image.copy().mean(axis=axis)))

	def row_mean(self):
		"""
		Mean for each row
		"""
		return(self.mean(axis=0))

	def col_mean(self):
		"""
		Column means
		"""
		return(self.mean(axis=1))

	def row_sum(self):
		"""
		Sums values for each row (across columns)
		"""
		# return(OCRImage(self.image.copy().sum(axis=0)))
		return(self.sum(axis=0))

	def col_sum(self):
		"""
		Returns sums for each column
		"""
		return(self.sum(axis=1))

	def sum(self, axis):
		"""
		Sum values in an image across a specified axis
		"""
		return(OCRImage(self.image.copy().sum(axis=axis)))

	def threshold(self, threshold=200, otsu=False):
		"""
		Outputs a binary mask of given threshold

		Tinkered a bit and a threshold of 200 seems reasonable for a few test images

		Might need to use an adaptive threshold depending on the image, however.
			- Something like a % of maximum value would probably work.
			- Let's go with a hard threshold, then adapt if we need to.

		CWB decided to use Otsu binary thresholding algo
			http://docs.opencv.org/master/d7/d4d/tutorial_py_thresholding.html#gsc.tab=0
		"""
		# thresh_image = self.image
		# thresh_mask = np.zeros(self.get_shape())
		# thresh_mask[thresh_image <= threshold]=True

		# return(OCRImage(thresh_mask))
		if otsu:
			tmp, thresh_image = cv2.threshold(self.image, threshold, 1, cv2.THRESH_BINARY_INV+cv2.THRESH_OTSU)
		else:
			tmp, thresh_image = cv2.threshold(self.image, threshold, 1, cv2.THRESH_BINARY_INV)

		
		# Need to invert the sign. Can't seem to get that right, despite setting thresh_binary_inv
		one_mask = thresh_image==1
		zero_mask = thresh_image==0
		thresh_image[one_mask]=0
		thresh_image[zero_mask]=1

		# thresh_image = (thresh_image-1)*-1 # this makes all zeros 1, and all ones 0

		return(OCRImage(thresh_image))

	def laplacian(self):
		"""
		Calculates Laplacian derivative

		http://opencv-python-tutroals.readthedocs.org/en/latest/py_tutorials/py_imgproc/py_gradients/py_gradients.html
		"""
		return(OCRImage(cv2.Laplacian(self.image, cv2.CV_8U)))

	def deskew(self):
		"""
		Deskew a digit so it's more "blocky".

		See http://docs.opencv.org/master/dd/d3b/tutorial_py_svm_opencv.html#gsc.tab=0
		"""
		pass

	def findContours(self, mode=cv2.RETR_TREE, method=cv2.CHAIN_APPROX_NONE):
		"""
		Find the contours of an image. Best results for binary images

		See below for reference
		http://docs.opencv.org/master/d4/d73/tutorial_py_contours_begin.html#gsc.tab=0
		http://docs.opencv.org/modules/imgproc/doc/structural_analysis_and_shape_descriptors.html
		"""
		image, contours, hierarchy = cv2.findContours(self.image, mode, method)

		# Return OCRImage object + contours + hierarchy
		return OCRImage(image), contours, hierarchy

	def to_pandas(self):
		"""
		Converts to a pandas data frame, with image#_row#_col# as index
		"""
		# reshape_image = np.reshape(self.image.copy(), np.prod(self.get_shape()), 1)

		# Initialize data frame
		df = pd.DataFrame()

		# Get file number, used in index below
		file_number = self.file_name.split('/')[1].split('.png')[0]
		row_labels = [str(y) for y in range(1, self.get_shape()[0]+1)]

		# labels = ['{0}_{1}_'.format(file_number, i) for i in row_labels]

		for col in range(self.get_shape()[1]):
			# row_labels = [str(y) for y in 1:self.get_shape()[0]]
			labels = ['{0}_{1}_{2}'.format(file_number, i, col+1) for i in row_labels]

			# Make a data frame
			entry = pd.DataFrame(data={'value': self.image[:,col]},
								 index=labels)

			# Append to growing data frame
			df = df.append(entry)
			
			# Grab the column we need
		# for column in range(self.get_shape()[1]):
		# 	# for row in range(self.get_shape()[0]):
		# 		print row, '_', column
		# 		entry = pd.DataFrame(data={'val': self.image[row][column]},
		# 							 index=[file_number + '_' + str(row) + '_' + str(column)])
		# 		df = df.append(entry)

		if np.prod(df.shape) != np.prod(self.get_shape()):
			print 'Error in: ' + self.file_name
			print self.get_shape(), df.shape
		return(df)

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
		image_to_plot= OCRImage(magnitude_spectrum)

		# Generate plot
		image_to_plot.plot_image()

		# Add colorbar
		plt.colorbar()


class ImageList:
	""" Arbitrary list of OCRImage objects """
	def __init__(self, image_list):
		self.image = list()

		# Build an image list from ... a list of file names ...
		for image in image_list:
			self.image.append(OCRImage(image))

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

		plt.show()

	def fft2(self):
		""" Compute fft2 for list of images """

		# Create a new image list
		fft_list = ImageList()

		for i in range(len(self.image)):
			fft_list.append(self.image[i].fft2().image)

		return(fft_list)


	def plot_image(self, figure_size=(10, 10)):
		"""
		Plot images
		"""

		plt.figure(figsize=figure_size)

		# Transform all images
		for i in range(len(self.image)):
			
			plt.subplot(len(self.image),1,i+1)
			
			self.image[i].plot_image()

		plt.show()

	def plot_mag(self, figure_size=(10, 10)):
		"""
		Plot magnitude specturm for all image
		"""

		plt.figure(figsize=figure_size)

		# Transform all images
		for i in range(len(self.image)):
			
			plt.subplot(len(self.image),1,i+1)
			
			self.image[i].plot_mag()

		plt.show()

	def canny_edge(self, min_val, max_val):
		"""
		Finds canny edge of images
		"""

		# for i in range(len(self.image)):
		pass

	def laplacian(self):
		"""
		Apply Laplacian to all images in list
		"""
		laplacian_list = ImageList([])

		for i in range(len(self.image)):
			laplacian_list.append(self.image[i].laplacian().image)
			laplacian_list.image[len(laplacian_list.image)-1].file_name = self.image[i].file_name

		return laplacian_list

	def split(self):
		"""
		Split images into two objects (e.g., train/test)

		use np.vsplit or equivalent, see 
		http://docs.opencv.org/master/d8/d4b/tutorial_py_knn_opencv.html#gsc.tab=0
		"""

	def threshold(self, threshold, otsu=False):
		"""
		WRapper for thresholding routine
		"""

		threshold_list = ImageList([])
		for i in range(len(self.image)):
			threshold_list.append(self.image[i].threshold(threshold, otsu).image)
			threshold_list.image[len(threshold_list.image)-1].file_name = self.image[i].file_name

		return threshold_list

	def to_csv(self, filename):
		"""
		Takes image list and exports to a CSV file

		Exports as image#_row#_col#, value

		Converts to pandas data frame
		"""

		# Convert each image to a pandas data frame and append them
		df = pd.DataFrame()
		for i in range(len(self.image)):
			# Print updates
			print self.image[i].file_name
			entry = self.image[i].to_pandas()

			# Append to growing data frame
			df = df.append(entry)

		# return df to testing
		df.to_csv(filename, index_label='id')

		return df
		

# Get list of training images
class TrainPair(ImageList):
	""" Loads clean/dirty images """

	def __init__(self, file_name):
		""" file_name: '14.png' """
		self.image = list()
		self.append('train_cleaned/' + file_name)
		self.append('train/' + file_name)
		
		
# Load the clean training data
# clean = ImageList(train_clean)
# dirty = ImageList(train_dirty)
test = ImageList(test)
# Here's my first attempt at image cleaning using a laplacian + adaptive thresholding approach
#	This seemed to work reasonably well! Much better than adaptive thresholding alone
#	It does, however, tend to make the letters too skinny. Maybe smoothing before thresholding
#	would also help?
# lap_otsu = test.laplacian().threshold(0, True).to_csv('lap_otsu.csv') # applies adaptive threshold, writes to file

	
		# Apply an approach
#	- Canny edge detection
#	- Smoothing + threshold
#	- 
