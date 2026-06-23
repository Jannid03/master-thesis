import cv2
import os
import subprocess
import sys

###Start
# print("Which XML file?")
# file = input()
file = sys.argv[1]

# print("Output folder name")
# name = input()

name = sys.argv[2]

# Define input and output paths

image_folder = f'D:\\Uni\\Masterarbeit\\data\\{name}'  # Replace with the path to your images
# print(image_folder)

video_name = f'{image_folder}\\movie.mp4' # Name of the output video file


##### Running of Morpheus
# result = subprocess.run(['C:\\Program Files\\Morpheus\\morpheus.exe', '-f', f'D:\\Uni\\Masterarbeit\\Morpheus_tries\\NF-KB_model\\{file}.xml', '--outdir', f'D:\\Uni\\Masterarbeit\\Morpheus_tries\\NF-KB_model\\{name}'], capture_output=True, text=True, shell=True)
# print(result.stdout)
# print(result.stderr)

process = subprocess.Popen(['C:\\Program Files\\Morpheus\\morpheus.exe', '-f', f'D:\\Uni\\Masterarbeit\\data\\{file}', '--outdir', f'D:\\Uni\\Masterarbeit\\data\\{name}', '--no-gnuplot'])
process.wait()
process.kill()
print("Morpheus Done. Starting video")
# Get list of images
images = [img for img in os.listdir(image_folder) if img.startswith("plot")]

# Read the first image to get the size
first_image = cv2.imread(os.path.join(image_folder, images[0]))
height, width, _ = first_image.shape

# Initialize video writer
fourcc = cv2.VideoWriter_fourcc(*'mp4v')  # Use 'mp4v' for MP4 format
video = cv2.VideoWriter(video_name, fourcc, 10, (width, height))

# Add images to the video
for image in images:
    img_path = os.path.join(image_folder, image)
    frame = cv2.imread(img_path)
    video.write(frame)

# Release the video writer
video.release()
cv2.destroyAllWindows()
print("Program done")
