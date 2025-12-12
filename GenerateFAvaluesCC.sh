#!/bin/bash

# Path to the folder containing subject folders
subjects_folder="/beegfs_data/scratch/iandrulyte-diffusion"

FA_folder="/beegfs_data/scratch/iandrulyte-diffusion/FA_maps/for_Ieva_FA_maps"

CC_divided_files="/beegfs_data/scratch/iandrulyte-diffusion/Divided_CC_three_parts"

# Create a CSV file for full AF FA stats with header
echo "subj,CC_part,min,max,mean" > "$subjects_folder"/CC_parts_fa_stats.csv

# Loop through subject folders starting with "t0"
for subj_folder in "$subjects_folder"/t0*
do
  # Check if the current item is a directory
  if [[ -d "$subj_folder" ]]; then
    for CC_part in "BCC" "GCC" "SCC"; do
      subj=$(basename "$subj_folder")

      echo "Generating binary mask for ${subj} ${CC_part}"

      #Generate binary mask of full AF
      scil_compute_streamlines_density_map.py "$CC_divided_files"/"$subj"__cc_homotopic_"$CC_part"_mni_space.trk --binary 1 "$CC_divided_files"/"$subj"__cc_homotopic_"$CC_part"_mni_space_binary_mask.nii.gz

      #Compute FA of the whole AF 

      echo "Computing FA for ${subj} ${CC_part}"

      fslmaths "$FA_folder"/"$subj"/"$subj"__fa_in_JHU_MNI.nii.gz -mas "$CC_divided_files"/"$subj"__cc_homotopic_"$CC_part"_mni_space_binary_mask.nii.gz "$FA_folder"/"$subj"/"$subj"__cc_homotopic_"$CC_part"_mni_space_FA_value.nii.gz

      # Get min, max, and mean FA values for full AF
      fa_stats=$(fslstats "$FA_folder"/"$subj"/"$subj"__cc_homotopic_"$CC_part"_mni_space_FA_value.nii.gz -R -M)

      # Extract min, max, and mean values for full AF
      min=$(echo "$fa_stats" | awk '{print $1}')
      max=$(echo "$fa_stats" | awk '{print $2}')
      mean=$(echo "$fa_stats" | awk '{print $3}')

      # Append to full AF FA stats CSV file
      echo "$subj,$CC_part,$min,$max,$mean" >> "$subjects_folder"/af_parts_fa_stats.csv

    done
  fi
done

