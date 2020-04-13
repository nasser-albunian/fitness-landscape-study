import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;

public class CalculateIA {

public String class_name = null;

	protected double threshold = 0.0;

	BufferedWriter bw;

	String identifier;

	public static void main(String[] args) throws IOException {
		CalculateIA obj = new CalculateIA();

		obj.setClassName(args[0]);

		obj.perform();
	}

	public void setClassName(String cls) {
		class_name = cls;
	}

	@SuppressWarnings("resource")
	private void perform() throws IOException {
		BufferedReader readerBuffer = null;

		File folder = new File(System.getProperty("user.dir")+"/Data/process/Transposed_data/");

		for (final File fileEntry : folder.listFiles()) {
			identifier = UUID.randomUUID().toString().substring(0, 8);

			String line;
			String path = folder.getPath()+"/"+fileEntry.getName();
			readerBuffer = new BufferedReader(new FileReader(path));

			String f = System.getProperty("user.dir")+"/Data/post_process/IA/" + class_name + ".csv";

			bw = new BufferedWriter(new FileWriter(f,true));

			if (f.length() == 0L) {
				bw.write("Identifier");
				bw.write(",");
				bw.write("Branch");
				bw.write(",");
				bw.write("Threshold");
				bw.write(",");
				bw.write("IC");
				bw.write(",");
				bw.write("PIC");
				bw.write(",");
				bw.write("DBI");
				bw.write(",");
				bw.write("\n");
			}

			int counter = 1;

			while ((line = readerBuffer.readLine()) != null) {
				if(!line.contains("NA")) {
					ArrayList<String> data_per_class = convertCSVtoArrayList(line);

					if(!data_per_class.get(0).equals("Class")) {
						List<String> fitness_values = data_per_class;

						List<Double> doubleList = fitness_values.stream()
								.map(Double::valueOf)
								.collect(Collectors.toList());

						this.calculateIA(doubleList, String.valueOf(counter));
					}
				}
				counter++;
			}
			bw.close();
		}
	}

	private void calculateIA(List<Double> fitnessValues, String class_name) throws IOException {

		List<Double> fitnessChanges = this.getFitnessChanges(fitnessValues);

		List<Double> sortedFitnessChanges = new ArrayList<Double>(new HashSet<Double>(fitnessChanges));

		double sum=0;

		for(double s: fitnessChanges) {sum += s;}
		double avg =  sum/fitnessChanges.size();

		double maxChange = fitnessChanges.stream().max(Comparator.comparing(Double::valueOf)).get();

		sortedFitnessChanges.add(0.0);
		sortedFitnessChanges.add(avg);

		Random generator = new Random();
	    for(int i=0; i<5; i++) {
	    	double number = generator.nextDouble() * maxChange;
	    	sortedFitnessChanges.add(number);
	    }

	    if(maxChange >= 1.0) {
	    	sortedFitnessChanges.add(maxChange - 1);
	    	sortedFitnessChanges.add(maxChange - 2);
	    	sortedFitnessChanges.add(maxChange - 3);
	    }else {
	    	sortedFitnessChanges.add(maxChange - 0.1);
	    	sortedFitnessChanges.add(maxChange - 0.2);
	    	sortedFitnessChanges.add(maxChange - 0.3);
	    }

		sortedFitnessChanges.removeIf(n -> (n < 0.0));

		Collections.sort(sortedFitnessChanges);

		for(double x: sortedFitnessChanges) {
			bw.write(identifier);
			bw.write(",");
			bw.write(class_name);
			bw.write(",");

			threshold = x;

			bw.write(String.valueOf(threshold));
			bw.write(",");

			List<Integer> slopeChanges = this.getSlopeChanges(fitnessChanges);

			List<Double> sixProbabilites = this.getSlopesProbabilities(slopeChanges);

			double IC = this.calculateIC(sixProbabilites);
			bw.write(String.valueOf(IC));
			bw.write(",");

			double PIC = this.calculatePartialIC(slopeChanges);
			bw.write(String.valueOf(PIC));
			bw.write(",");

			List<Double> threeProbabilites = this.getSlopesThreeProbabilities(slopeChanges);

			double DBI = this.calculateDensityBasinInformation(threeProbabilites);
			bw.write(String.valueOf(DBI));
			bw.write("\n");
		}
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

	private List<Double> getFitnessChanges(List<Double> parentsarray){
		List<Double> fitnessChanges = new ArrayList<Double>();

		for(int i=1; i<parentsarray.size(); i++) {
			fitnessChanges.add(parentsarray.get(i) - parentsarray.get(i-1));
		}

		return fitnessChanges;
	}

	private List<Integer> getSlopeChanges(List<Double> fitnessChanges){
		List<Integer> slopeChanges = new ArrayList<Integer>();

		for(double s : fitnessChanges) {
			if(s < -threshold) {slopeChanges.add(-1);}
			else if(Math.abs(s) <= threshold) {slopeChanges.add(0);}
			else if(s > threshold) {slopeChanges.add(1);}
		}

		return slopeChanges;
	}

	private List<Double> getSlopesProbabilities(List<Integer> slopeChanges){
		List<Double> sixProbabilites = new ArrayList<Double>();

		int count_neg10 = 0, count_neg11 = 0, count_10 = 0, count_1neg1 = 0, count_0neg1 = 0, count_01 = 0;

		for(int i=0; i<slopeChanges.size(); i++) {
			if(i+1 != slopeChanges.size()) {
				if((slopeChanges.get(i) == -1) && (slopeChanges.get(i+1) == 0)) {count_neg10++;}
				else if((slopeChanges.get(i) == -1) && (slopeChanges.get(i+1) == 1)) {count_neg11++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(i+1) == 0)) {count_10++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(i+1) == -1)) {count_1neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(i+1) == -1)) {count_0neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(i+1) == 1)) {count_01++;}
			}else {
				if((slopeChanges.get(i) == -1) && (slopeChanges.get(0) == 0)) {count_neg10++;}
				else if((slopeChanges.get(i) == -1) && (slopeChanges.get(0) == 1)) {count_neg11++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(0) == 0)) {count_10++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(0) == -1)) {count_1neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(0) == -1)) {count_0neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(0) == 1)) {count_01++;}
			}
		}

		if(count_neg10 != 0) {sixProbabilites.add(count_neg10 / ((double)slopeChanges.size()));}
		if(count_neg11 != 0) {sixProbabilites.add(count_neg11 / ((double)slopeChanges.size()));}
		if(count_10 != 0) {sixProbabilites.add(count_10 / ((double)slopeChanges.size()));}
		if(count_1neg1 != 0) {sixProbabilites.add(count_1neg1 / ((double)slopeChanges.size()));}
		if(count_0neg1 != 0) {sixProbabilites.add(count_0neg1 / ((double)slopeChanges.size()));}
		if(count_01 != 0) {sixProbabilites.add(count_01 / ((double)slopeChanges.size()));}

		return sixProbabilites;
	}

	private List<Double> getSlopesThreeProbabilities(List<Integer> slopeChanges){
		List<Double> threeProbabilites = new ArrayList<Double>();

		int count_neg1neg1 = 0, count_00 = 0, count_11 = 0;

		for(int i=0; i<slopeChanges.size(); i++) {
			if(i+1 != slopeChanges.size()) {
				if((slopeChanges.get(i) == -1) && (slopeChanges.get(i+1) == -1)) {count_neg1neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(i+1) == 0)) {count_00++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(i+1) == 1)) {count_11++;}
			}else {
				if((slopeChanges.get(i) == -1) && (slopeChanges.get(0) == -1)) {count_neg1neg1++;}
				else if((slopeChanges.get(i) == 0) && (slopeChanges.get(0) == 0)) {count_00++;}
				else if((slopeChanges.get(i) == 1) && (slopeChanges.get(0) == 1)) {count_11++;}
			}
		}

		if(count_11 != 0) {threeProbabilites.add((double)count_11 / ((double)slopeChanges.size()));}
		if(count_neg1neg1 != 0) {threeProbabilites.add((double)count_neg1neg1 / ((double)slopeChanges.size()));}
		if(count_00 != 0) {threeProbabilites.add((double)count_00 / ((double)slopeChanges.size()));}

		return threeProbabilites;
	}

	private double calculateIC(List<Double> sixProbabilites) {
		double H=0;

		for(Double s: sixProbabilites) {
			double logS = (Math.log(s) / Math.log(6));
			if(logS == Double.NEGATIVE_INFINITY || logS == Double.POSITIVE_INFINITY) {logS = 0;}
			H -= (s * logS);
		}

		return H;
	}

	private double calculatePartialIC(List<Integer> slopeChanges) {
		double PIC=0;

		List<Integer> slopeChangesFiltered = new ArrayList<Integer>();

		int temp=0;
		for(int i=0; i<slopeChanges.size(); i++) {
			if(i+1 != slopeChanges.size()) {
				if((slopeChanges.get(i) != 0) && (slopeChanges.get(i) != temp)) {
					slopeChangesFiltered.add(slopeChanges.get(i));
					temp=slopeChanges.get(i);
				}
			}else {
				if((slopeChanges.get(i) != 0) && (slopeChanges.get(i) != temp)) {
					slopeChangesFiltered.add(slopeChanges.get(i));
					temp=slopeChanges.get(i);
				}
			}
		}

		PIC = (double)slopeChangesFiltered.size() / (double)slopeChanges.size();

		return PIC;
	}

	private double calculateDensityBasinInformation(List<Double> threeProbabilites) {
		double H=0;

		for(Double s: threeProbabilites) {
			double logS = (Math.log(s) / Math.log(3));
			if(logS == Double.NEGATIVE_INFINITY || logS == Double.POSITIVE_INFINITY) {logS = 0;}
			H -= (s * logS);
		}

		return H;
	}
}
