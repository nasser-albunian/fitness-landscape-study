import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class CalculateAC {

	public String class_name = null;

	public static void main(String[] args) {

		CalculateAC obj = new CalculateAC();

		obj.setClassName(args[0]);

		obj.applyForCase();

	}

	public void setClassName(String cls) {
		class_name = cls;
	}

	public void applyForCase() {

		CalculateAC obj = new CalculateAC();

		BufferedReader readerBuffer = null;
		BufferedReader readerBufferR = null;

		BufferedWriter bw = null;

		File folder = new File(System.getProperty("user.dir")+"/Data/process/Transposed_data/");

		int fileCounter = 0;

		for (final File fileEntry : folder.listFiles()) {
			fileCounter++;
			List<Double> results = new ArrayList<Double>();
			try {
				String line;
				String path = folder.getPath()+"/"+fileEntry.getName();

				readerBuffer = new BufferedReader(new FileReader(path));

				int count=0;

				while ((line = readerBuffer.readLine()) != null) {
					if(!line.contains("NA")) { //sometimes RW files have a line of NAs at the end
						ArrayList<String> data_per_class = convertCSVtoArrayList(line);

						List<Double> doubleList = data_per_class.stream()
								.map(Double::valueOf)
								.collect(Collectors.toList());

						for(int i=1; i<=1; i++) {
							double AC = obj.calculateAC(doubleList, i);
							results.add(AC);
						}
					}
				}

				// writing to CSV

				String resultsPath = System.getProperty("user.dir")+"/Data/post_process/AC/";

				String file_name = class_name + ".csv";
				String file_name1 = class_name + "1.csv";

				File f = new File(resultsPath+file_name);

				if(f.exists()) {
					bw = new BufferedWriter(new FileWriter(resultsPath+file_name1,true));
					readerBufferR = new BufferedReader(new FileReader(f));
					int i=0;
					while ((line = readerBufferR.readLine()) != null) {
						bw.write(line+String.valueOf(results.get(i)+","));
						i++;
						bw.write("\n");
					}
					bw.close();
					f.delete();
					Path source = Paths.get(resultsPath+file_name1);
					Files.move(source, source.resolveSibling(file_name));
				}else {
					bw = new BufferedWriter(new FileWriter(resultsPath+file_name,true));
					for(int i=0; i<results.size(); i++) {
						count++;
						bw.write(String.valueOf(count));
						bw.write(",");
						bw.write(String.valueOf(results.get(i)));
						bw.write(",");
						bw.write("\n");
					}
					bw.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private double calculateAC(List<Double> individuals, int step) {
		double mean = this.calculateMean(individuals);

		int n = individuals.size();
		int step_size = step; //step (leg) size

		double sum1 = 0.0, sum2 =0.0;

		for(int i=0; i<n-step_size; i++) {
			sum1 += (individuals.get(i)-mean)*(individuals.get(i+step_size)-mean);
		}

		for(int i=0; i<n; i++) {
			sum2 += (individuals.get(i)-mean)*(individuals.get(i)-mean);
		}

		double autoCorrelationValue = sum1/sum2;

		if(Double.isNaN(autoCorrelationValue)) {
			autoCorrelationValue = 1.0;
		}

		return autoCorrelationValue;
	}

	private double calculateMean(List<Double> individuals) {
		double sum = 0;
		for(int i=0; i<individuals.size(); i++) {
			sum += individuals.get(i);
		}
		return sum/individuals.size();
	}


	public static ArrayList<String> convertCSVtoArrayList(String pathCSV) {
		ArrayList<String> result = new ArrayList<String>();

		if (pathCSV != null) {
			String[] splitData = pathCSV.split("\\s*,\\s*");
			for (int i = 0; i < splitData.length; i++) {
				if (!(splitData[i] == null) || !(splitData[i].length() == 0)) {
					result.add(splitData[i].trim().replace("\"", ""));
				}
			}
		}

		return result;
	}


	public double getStep(List<Double> doubleList, int step) {
		double sum = doubleList.get(0) + doubleList.get(step);
		return sum;
	}
}
