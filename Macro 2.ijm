waitForUser("Welcome!", "This macro is adapted for a directory (input) containing 1-layer 1-channel tif images originating from a file with .lif extension containing tile scans for 2 channels: ch00, ch01. \nEventually other channels may be present but they will not be used here.\n\nIt generates: \n- a mutichannel tif file per tile scan (fused). \nThe image name format must be <[name]_[x]x[y]_[leica automatic additions]> where x and y are width and height of the tile scan. \nProcess together only tile scans of the same size. \n \nFirst, test this macro on a directory of 2 tile scans of your biggest and smallest samples.\nCheck carefully the next popping up window! \n \nReady? Press the button and play =).\nSincerely yours, Z.Alex");

setBatchMode(true);

//Call image processing parameters
Dialog.create("Specify your image processing parameters");
Dialog.addNumber("grid width", 2, 0, 10, "tiles");
Dialog.addNumber("grid height", 2, 0, 10, "tiles");
Dialog.addNumber("ch00 min", 0, 0, 10, "");
Dialog.addNumber("ch00 max", 256, 0, 10, "");
Dialog.addNumber("ch01 min", 0, 0, 10, "");
Dialog.addNumber("ch01 max", 256, 0, 10, "");
Dialog.show();
x = Dialog.getNumber();
y = Dialog.getNumber();
ch00_min = Dialog.getNumber();
ch00_max = Dialog.getNumber();
ch01_min = Dialog.getNumber();
ch01_max = Dialog.getNumber();

//Separate images by channel
input = getDirectory("Choose the input directory");
output = getDirectory("Choose the output directory");
File.makeDirectory(output+"ch00");
ch00 = output+"/ch00/";
File.makeDirectory(output+"ch01");
ch01 = output+"/ch01/";
images = getFileList(input);
for (i = 0; i < lengthOf(images); i++) {
	open(input+images [i]);
	name = getTitle();
	if (endsWith(name, "ch00.tif")) {
		open(input+name);
		saveAs("Tiff", ch00+name);
	}
	else if (endsWith(name, "ch01.tif")) {
		open(input+name);
		saveAs("Tiff", ch01+name);
	}
}

//Create multichannel tiles
File.makeDirectory(output+"multi");
multi = output+"/multi/";
images_ch00 = getFileList(ch00);
images_ch01 = getFileList(ch01);
for (i = 0; i < lengthOf(images_ch00); i++) {
	open(ch00+images_ch00 [i]);
	name_ch00 = getTitle();
	open(ch01+images_ch01 [i]);
	name_ch01 = getTitle();
	run("Merge Channels...", "c1="+name_ch00+" c2="+name_ch01+" create");
	name = replace(name_ch00, "_z0_ch00", "");
	name = replace(name, ".lif", "");
	rename(name);
	Stack.setDisplayMode("grayscale");
	saveAs("Tiff", multi+name);
}

//Delete intermediate folders
files_ch00 = getFileList(ch00);
files_ch01 = getFileList(ch01);
for (i = 0; i < lengthOf(files_ch00); i++) {
	File.delete(ch00+files_ch00[i]);
	File.delete(ch00);
}
for (i = 0; i < lengthOf(files_ch01); i++) {
	File.delete(ch01+files_ch01[i]);
	File.delete(ch01);
}

//Separate images by sample
File.makeDirectory(output+"tiles");
tiles = output+"/tiles/";
images_multi = getFileList(multi);
for (i = 0; i < lengthOf(images_multi); i++) {
	open(multi+images_multi[i]);
	name = getTitle();
	name_length = lengthOf(name);
	if (x*y < 10) {
		subname_length = name_length-11;
	}
	else {
		subname_length = name_length-12;
	}
	subname = substring(name, 0, subname_length);
	File.makeDirectory(tiles+subname);
	subname_dir = tiles+"/"+subname+"/";
	save_name_length = name_length-4;
	save_name = substring(name, 0, save_name_length);
	saveAs("Tiff", subname_dir+save_name);
}

//Delete intermediate folders
files_multi = getFileList(multi);
for (i = 0; i < lengthOf(files_multi); i++) {
	File.delete(multi+files_multi[i]);
	File.delete(multi);
}

//Stitch tiles and analyse fused images
File.makeDirectory(output+"fused");
fused = output+"/fused/";
montage = output+"/montage/";
images_tiles = getFileList(tiles);
for (i = 0; i < lengthOf(images_tiles); i++) {
	
	//Stitch
	subtiles = tiles+"/"+images_tiles[i]+"/";
	images_subtiles = getFileList(subtiles);
	open(subtiles+images_subtiles[0]);
	first_tile_name = getTitle();
	first_tile_name_length = lengthOf(first_tile_name);
	if (x*y < 10) {
		first_tile_subname_length = first_tile_name_length-6;
	}
	else {
		first_tile_subname_length = first_tile_name_length-7;
	}
	first_tile_subname = substring(first_tile_name, 0, first_tile_subname_length);

	setBatchMode(false);

	if (x*y < 10) {
		n = "i";
	}
	else {
		n = "ii";
	}
	run("Grid/Collection stitching", "type=[Grid: row-by-row] order=[Right & Up] grid_size_x="+x+" grid_size_y="+y+" tile_overlap=20 first_file_index_i=0 directory="+subtiles+" file_names="+first_tile_subname+"s{"+n+"}.tif output_textfile_name=TileConfiguration.txt fusion_method=[Linear Blending] regression_threshold=0.30 max/avg_displacement_threshold=2.50 absolute_displacement_threshold=3.50 compute_overlap computation_parameters=[Save memory (but be slower)] image_output=[Fuse and display]");
	name_length = first_tile_subname_length-5;
	name = substring(first_tile_subname, 0, name_length);
	rename(name);
	Stack.setDisplayMode("color");

	//Analyse ch00
	setSlice(1);
	setMinAndMax(ch00_min, ch00_max);
	run("Fire");

	//Analyse ch01
	setSlice(2);
	setMinAndMax(ch01_min, ch01_max);
	run("Fire");

	//Save multichannel
	saveAs("Tiff", fused+name+".tif");

	run("Close All");
}

//Delete intermediate folders
files_tiles = getFileList(tiles);
for (i = 0; i < lengthOf(files_tiles); i++) {
	files_tiles_dir = tiles+"/"+files_tiles[i]+"/";
	subfiles_tiles = getFileList(files_tiles_dir);
	for (j = 0; j < lengthOf(subfiles_tiles); j++) {
		File.delete(files_tiles_dir+subfiles_tiles[j]);
		File.delete(files_tiles_dir);
	}
	File.delete(files_tiles_dir);
	File.delete(tiles);
}