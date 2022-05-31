//Open manually your images

//Call Brigtness and Contrast settings
Dialog.create("Specify your Brigtness and Contrast settings");
Dialog.addString("Min", "15", 35);
Dialog.addString("Max", "135", 35);
Dialog.show();
min = Dialog.getString();
max = Dialog.getString();

//Call montage settings
Dialog.create("Specify your montage settings");
Dialog.addMessage("Number of columns * Number of rows should be equal to the number of images +(0-1)");
Dialog.addString("Number of columns", "2", 30);
Dialog.addString("Number of rows", "3", 30);
Dialog.show();
columns = Dialog.getString();
rows = Dialog.getString();

//Create output directories
input = getDirectory("input");
output = getDirectory("output");
outputname = File.getName(output);
File.makeDirectory(output+"/jpeg");
jpeg = output+"/jpeg/";

//For each opened image
images = getList("image.titles");
n = lengthOf(images);
for(i=0; i<n; i++) {
	selectWindow(images[i]);
	name = getTitle();

	//Analyse
	run("Brightness/Contrast...");
	setMinAndMax(min, max);
	
	//Save as jpeg
	saveAs("Jpeg", jpeg+name);
}

//Plot all images in a montage
run("Images to Stack", "name=Stack title=[] use");
run("Make Montage...", "columns="+columns+" rows="+rows+" scale=1 border=1 font=100");
saveAs("Tiff", output+outputname);

//Close all images
run("Close All")