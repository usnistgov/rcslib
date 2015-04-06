/*
The NIST RCS (Real-time Control Systems)
library is public domain software, however it is preferred
that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified. 

 */


package diagapplet.utils;

import java.awt.Component;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.swing.JFileChooser;

/**
 * Set of utilities for saving images.
 * @author Will Shackleford {@literal <william.shackleford@nist.gov>}
 */
public class SaveImage {

    static private File last_dir = null;
    static private int save_as_img_count = 0;
    
    public enum BAYER_PATTERN { BGGR, GRBG, RGGB, GBRG };
    

    static public byte[] bayer_NearestNeighbor(final byte bayer[],
            final int width,
            final int height,
            final BAYER_PATTERN pattern) {
        byte rgb[] = new byte[width * height * 3];

        boolean row_starts_with_green = (pattern == BAYER_PATTERN.GBRG || pattern == BAYER_PATTERN.GRBG);
        boolean red_row = (pattern == BAYER_PATTERN.RGGB || pattern == BAYER_PATTERN.GRBG);

        int rgb_index = 0;
        int bayer_index = 0;
        boolean even_col = true;
        for (int i = 0; i < height - 1; i++) {
            for (int j = 0; j < width - 1; j++, rgb_index += 3, bayer_index++, even_col = !even_col) {
                if (row_starts_with_green == even_col) {
                    rgb[rgb_index + 1] = bayer[bayer_index];
                    if (red_row) {
                        rgb[rgb_index + 0] = bayer[bayer_index + 1];
                        rgb[rgb_index + 2] = bayer[bayer_index + width];
                    } else {
                        rgb[rgb_index + 0] = bayer[bayer_index + width];
                        rgb[rgb_index + 2] = bayer[bayer_index + 1];
                    }
                } else {
                    rgb[rgb_index + 1] = bayer[bayer_index + 1];
                    if (red_row) {
                        rgb[rgb_index + 0] = bayer[bayer_index];
                        rgb[rgb_index + 2] = bayer[bayer_index + width + 1];
                    } else {
                        rgb[rgb_index + 0] = bayer[bayer_index + width + 1];
                        rgb[rgb_index + 2] = bayer[bayer_index];
                    }
                }
            }
            red_row = !red_row;
            row_starts_with_green = !row_starts_with_green;
            rgb_index += 3;
            bayer_index++;
            even_col = true;
        }

        // Add the last column on the right.
        row_starts_with_green = (pattern == BAYER_PATTERN.GBRG || pattern == BAYER_PATTERN.GRBG);
        red_row = (pattern == BAYER_PATTERN.RGGB || pattern == BAYER_PATTERN.GRBG);
        rgb_index = 3 * (width - 1);
        bayer_index = width - 1;
        final int rgb_index_step = 3 * width;
        for (int i = 0; i < height - 1; i++, rgb_index += rgb_index_step, bayer_index += width) {
            if (row_starts_with_green == even_col) {
                rgb[rgb_index + 1] = bayer[bayer_index];
                if (red_row) {
                    rgb[rgb_index + 0] = bayer[bayer_index - 1];
                    rgb[rgb_index + 2] = bayer[bayer_index + width];
                } else {
                    rgb[rgb_index + 0] = bayer[bayer_index + width];
                    rgb[rgb_index + 2] = bayer[bayer_index - 1];
                }
            } else {
                rgb[rgb_index + 1] = bayer[bayer_index - 1];
                if (red_row) {
                    rgb[rgb_index + 0] = bayer[bayer_index];
                    rgb[rgb_index + 2] = bayer[bayer_index + width - 1];
                } else {
                    rgb[rgb_index + 0] = bayer[bayer_index + width - 1];
                    rgb[rgb_index + 2] = bayer[bayer_index];
                }
            }
            red_row = !red_row;
            row_starts_with_green = !row_starts_with_green;
        }

        // Add the Bottom row
        rgb_index = (width * (height - 1)) * 3;
        bayer_index = (width * (height - 1));
        for (int j = 0; j < width - 1; j++, even_col = !even_col, rgb_index += 3, bayer_index++) {
            if (row_starts_with_green == even_col) {
                rgb[rgb_index + 1] = bayer[bayer_index];
                if (red_row) {
                    rgb[rgb_index + 0] = bayer[bayer_index + 1];
                    rgb[rgb_index + 2] = bayer[bayer_index - width];
                } else {
                    rgb[rgb_index + 0] = bayer[bayer_index - width];
                    rgb[rgb_index + 2] = bayer[bayer_index + 1];
                }
            } else {
                rgb[rgb_index + 1] = bayer[bayer_index - 1];
                if (red_row) {
                    rgb[rgb_index + 0] = bayer[bayer_index];
                    rgb[rgb_index + 2] = bayer[bayer_index - width + 1];
                } else {
                    rgb[rgb_index + 0] = bayer[bayer_index - width + 1];
                    rgb[rgb_index + 2] = bayer[bayer_index];
                }
            }
        }

        // Add bottom right pixel
        if (row_starts_with_green == even_col) {
            rgb[rgb_index + 1] = bayer[bayer_index];
            if (red_row) {
                rgb[rgb_index + 0] = bayer[bayer_index - 1];
                rgb[rgb_index + 2] = bayer[bayer_index - width];
            } else {
                rgb[rgb_index + 0] = bayer[bayer_index - width];
                rgb[rgb_index + 2] = bayer[bayer_index - 1];
            }
        } else {
            rgb[rgb_index + 1] = bayer[bayer_index - 1];
            if (red_row) {
                rgb[rgb_index + 0] = bayer[bayer_index];
                rgb[rgb_index + 2] = bayer[bayer_index - width - 1];
            } else {
                rgb[rgb_index + 0] = bayer[bayer_index - width - 1];
                rgb[rgb_index + 2] = bayer[bayer_index];
            }
        }

        return rgb;
//        final int bayerStep = sx;
//        final int rgbStep = 3 * sx;
//        int width = sx;
//        int height = sy;
//        int blue = ((tile == BAYER_PATTERN_BGGR) || (tile == BAYER_PATTERN_GBRG)) ? -1 : 1;
//        boolean start_with_green =
//                (tile == BAYER_PATTERN_GBRG || tile == BAYER_PATTERN_GRBG);
//        int i, imax, iinc;
//
//        /* add black border */
//        imax = sx * sy * 3;
//        for (i = sx * (sy - 1) * 3; i < imax; i++) {
//            rgb[i] = 0;
//        }
//        iinc = (sx - 1) * 3;
//        int rgb_index = 0;
//        int bayer_index = 0;
//        for (i = (sx - 1) * 3; i < imax; i += iinc) {
//            rgb[i++] = 0;
//            rgb[i++] = 0;
//            rgb[i++] = 0;
//        }
//
//        rgb_index += 1;
//        width -= 1;
//        height -= 2;
//
//        for (; height > 0; height--, bayer_index += bayerStep, rgb_index += rgbStep) {
//            //int t0, t1;
//            final int bayerEnd = width;
//
//            if (start_with_green) {
//                rgb[rgb_index - blue] = bayer[bayer_index + 1];
//                rgb[rgb_index + 0] = bayer[bayer_index + bayerStep + 1];
//                rgb[rgb_index + blue] = bayer[bayer_index + bayerStep];
//                bayer_index++;
//                rgb_index += 3;
//            }
//
//            if (blue > 0) {
//                for (; bayer_index <= bayerEnd - 2; bayer_index += 2, rgb_index += 6) {
//                    rgb[rgb_index - 1] = bayer[bayer_index + 0];
//                    rgb[rgb_index + 0] = bayer[bayer_index + 1];
//                    rgb[rgb_index + 1] = bayer[bayer_index + bayerStep + 1];
//
//                    rgb[rgb_index + 2] = bayer[bayer_index + 2];
//                    rgb[rgb_index + 3] = bayer[bayer_index + bayerStep + 2];
//                    rgb[rgb_index + 4] = bayer[bayer_index + bayerStep + 1];
//                }
//            } else {
//                for (; bayer_index <= bayerEnd - 2; bayer_index += 2, rgb_index += 6) {
//                    rgb[rgb_index + 1] = bayer[bayer_index + 0];
//                    rgb[rgb_index + 0] = bayer[bayer_index + 1];
//                    rgb[rgb_index - 1] = bayer[bayer_index + bayerStep + 1];
//
//                    rgb[rgb_index + 4] = bayer[bayer_index + 2];
//                    rgb[rgb_index + 3] = bayer[bayer_index + bayerStep + 2];
//                    rgb[rgb_index + 2] = bayer[bayer_index + bayerStep + 1];
//                }
//            }
//
//            if (bayer_index < bayerEnd) {
//                rgb[rgb_index - blue] = bayer[bayer_index + 0];
//                rgb[rgb_index + 0] = bayer[bayer_index + 1];
//                rgb[rgb_index + blue] = bayer[bayer_index + bayerStep + 1];
//                bayer_index++;
//                rgb_index += 3;
//            }
//
//            bayer_index -= width;
//            rgb_index -= width * 3;
//
//            blue = -blue;
//            start_with_green = !start_with_green;
//        }
//        return rgb;
    }

    static public BufferedImage BayerByteArrayToImage(final byte bayer_ba[], final int width, final int height, final BAYER_PATTERN pattern) throws Exception {
        if (width < 1 || height < 1 || bayer_ba.length < (width * height)) {
            throw new Exception("Bad argumements ba.length(" + bayer_ba.length + ")  must be greater than width(" + width + ") X height(" + height + ")  =" + (width * height));
        }
        byte rgb[] = bayer_NearestNeighbor(bayer_ba, width, height, pattern);
        return ByteArrayToImage(rgb, width, height);
    }

    static public BufferedImage ByteArrayToImage(final byte ba[], final int width, final int height) throws Exception {
        if (width < 1 || height < 1 || ba.length < (width * height * 3)) {
            throw new Exception("Bad argumements ba.length(" + ba.length + ")  must be greater than width(" + width + ") X height(" + height + ") X 3 =" + (width * height * 3));
        }
        BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        int array_index = 0;
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++, array_index += 3) {
                int rgb_int = ((ba[array_index] & 0xFF) << 16) |
                        ((ba[array_index + 1] & 0xFF) << 8) |
                        ((ba[array_index + 2] & 0xFF));
                bi.setRGB(j, i, rgb_int);
            }
        }
        return bi;
    }

    static public BufferedImage BayerByteArrayToImage(final byte ba[], final int width, final int height) throws Exception {
        if (width < 1 || height < 1 || ba.length < (width * height * 3)) {
            throw new Exception("Bad argumements ba.length(" + ba.length + ")  must be greater than width(" + width + ") X height(" + height + ") X 3 =" + (width * height * 3));
        }
        BufferedImage bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                final int array_index = (i * width + j) * 3;
                int rgb_int = ((ba[array_index] & 0xFF) << 16) |
                        ((ba[array_index + 1] & 0xFF) << 8) |
                        ((ba[array_index + 2] & 0xFF));
                bi.setRGB(j, i, rgb_int);
            }
        }
        return bi;
    }

    static public void SaveImageAs(Image img, Component parent) {
        try {
            save_as_img_count++;
            final String suffixes[] = ImageIO.getWriterFileSuffixes();
            String suffix_list = suffixes[0];
            for (int i = 1; i < suffixes.length; i++) {
                suffix_list += "," + suffixes[i];
            }
            final String suffix_list_final = suffix_list;

            javax.swing.filechooser.FileFilter filter = new javax.swing.filechooser.FileFilter() {

                public boolean accept(File f) {
                    if (f.isDirectory()) {
                        return true;
                    }
                    for (int i = 0; i < suffixes.length; i++) {
                        String s = suffixes[i];
                        if (f.getName().endsWith(s)) {
                            return true;
                        }
                    }
                    return false;
                }

                public String getDescription() {
                    return "Image Files: " + suffix_list_final;
                }
            };
            JFileChooser chooser = new JFileChooser();
            if (last_dir != null) {
                chooser.setCurrentDirectory(last_dir);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
            chooser.setFileFilter(filter);
            int hc = img.hashCode();
            String fname = String.format("%6d_%d.jpg", hc, save_as_img_count);
            chooser.setSelectedFile(new File(chooser.getCurrentDirectory(), fname));
            int returnVal = chooser.showSaveDialog(parent);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                String fpath = chooser.getSelectedFile().getPath();
                System.out.println("You chose to save the image in this file: " +
                        fpath);
                last_dir = chooser.getCurrentDirectory();
                SaveImage.SaveImageFileBySuffix(img, chooser.getSelectedFile().getAbsolutePath());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void SaveImageFileBySuffix(Image img_in, String filename) throws IOException {
        final String suffixes[] = ImageIO.getWriterFileSuffixes();
        String suffix = filename.substring(filename.lastIndexOf('.'));
        for (int i = 0; i < suffixes.length; i++) {
            if (filename.endsWith(suffixes[i])) {
                suffix = suffixes[i];
                break;
            }
        }
        // System.out.println("suffix = " + suffix);
        String formats[] = ImageIO.getWriterFormatNames();
        String format = formats[0];
        boolean format_found = false;
        for (int i = 0; i < formats.length && !format_found; i++) {
            // System.out.println("i = " + i);
            // System.out.println("formats[i] = " + formats[i]);
            Iterator iw_by_suffix_it = ImageIO.getImageWritersBySuffix(suffix);
            while (iw_by_suffix_it.hasNext() && !format_found) {
                ImageWriter iws = (ImageWriter) iw_by_suffix_it.next();
                // System.out.println("iws = " + iws);
                Iterator iw_by_format_it = ImageIO.getImageWritersByFormatName(formats[i]);
                while (iw_by_format_it.hasNext() && !format_found) {
                    ImageWriter iwf = (ImageWriter) iw_by_format_it.next();
                    // System.out.println("iwf = " + iwf);
                    if (iws.getClass().equals(iwf.getClass())) {
                        format = formats[i];
                        format_found = true;
                        break;
                    }
                }
            }
        }
        // System.out.println("format_found = " + format_found);
        int new_width = img_in.getWidth(null);
        int new_height = img_in.getHeight(null);
        int w_inc = 0;
        int h_inc = 0;
        if (new_width % 16 != 0) {
            w_inc = 16 - (new_width % 16);
            new_width += w_inc;
        }
        if (new_height % 16 != 0) {
            h_inc = 16 - (new_height % 16);
            new_height += h_inc;
        }
        BufferedImage img_to_record = new BufferedImage(new_width,
                new_height,
                BufferedImage.TYPE_INT_RGB);
        img_to_record.getGraphics().drawImage(img_in, (w_inc / 2), (h_inc / 2), null);
        // System.out.println("Saving to " + filename + " as " + format);
        ImageIO.write(img_to_record, format, new File(filename));
    }
}
