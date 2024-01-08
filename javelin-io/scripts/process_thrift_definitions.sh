set -e

pinch-gen --in="javelin-io/third-party/parquet.thrift" \
          --out="javelin-io/src" \
          --module-prefix="Data.Series.Generic.IO." \
          --hashable-vec-mod="Data.Vector.Instances" \
          --no-generate-arbitrary \
          --no-generate-nfdata

outdir="javelin-io/src/Data/Series/Generic/IO/Parquet/"

# There is no need to serialize/deserialize Parquet types
# at this time
rm -f "$outdir/Client.hs"
rm -f "$outdir/Server.hs"

# Make it clear that some files are autogenerated
sed -i -e "1i -- This file was autogenerated by the $(basename "$0") script" "$outdir/Types.hs"