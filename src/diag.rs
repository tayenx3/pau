use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub path: String,
    pub primary_err: String,
    pub primary_span: Span,
    pub secondary_messages: Vec<(Option<String>, Span)>,
}

pub fn determine_line(line_starts: &[usize], pos: usize) -> usize {
    match line_starts.binary_search(&pos) {
        Ok(line) => line,
        Err(line) => line.saturating_sub(1),
    }
}

pub fn create_src_ref(output: &mut String, digits_len: u32, path: &str, start_line: usize, start_col: usize, end_line: usize, end_col: usize, lines: &[&str]) {
    use colored::Colorize;

    output.push_str(
        &format!("{}{} {} {}\n", " ".repeat(digits_len as usize + 1), "╭──".cyan().bold(),
        format!("{}:{}:{}", path, start_line + 1, start_col + 1).italic(),
        "──".cyan().bold()),
    );
    output.push_str(&format!("{:w$} {}\n", " ", "│".cyan().bold(), w = digits_len as usize));

    for l_id in start_line..=end_line {
        let ltxt = lines[l_id];
        output.push_str(&format!("{:w$} {} {ltxt}\n", (l_id + 1).to_string().cyan().bold(), "│".cyan().bold(), w = digits_len as usize));

        let mut carets = String::new();
        if l_id == start_line && l_id == end_line {
            carets.push_str(&" ".repeat(start_col));
            carets.push_str(&"^".repeat(end_col - start_col).bright_red().bold().to_string());
        } else if l_id == start_line {
            carets.push_str(&" ".repeat(start_col));
            carets.push_str(&"^".repeat(ltxt.len() - start_col).bright_red().bold().to_string());
        } else if l_id == end_line {
            carets.push_str(&"^".repeat(end_col).bright_red().bold().to_string());
        } else {
            carets.push_str(&"^".repeat(ltxt.len()).bright_red().bold().to_string());
        }
        output.push_str(&format!("{:w$} {} {}\n", " ", "│".cyan().bold(), carets, w = digits_len as usize));
    }

    output.push_str(&format!("{:w$} {}\n", " ", "│".cyan().bold(), w = digits_len as usize));
}

pub fn create_src_ref_without_path(output: &mut String, digits_len: u32, start_line: usize, start_col: usize, end_line: usize, end_col: usize, lines: &[&str]) {
    use colored::Colorize;

    output.push_str(&format!("{:w$} {}\n", " ", "│".cyan().bold(), w = digits_len as usize));

    for l_id in start_line..=end_line {
        let ltxt = lines[l_id];
        output.push_str(&format!("{:w$} {} {ltxt}\n", (l_id + 1).to_string().cyan().bold(), "│".cyan().bold(), w = digits_len as usize));

        let mut carets = String::new();
        if l_id == start_line && l_id == end_line {
            carets.push_str(&" ".repeat(start_col));
            carets.push_str(&"^".repeat(end_col - start_col).bright_red().bold().to_string());
        } else if l_id == start_line {
            carets.push_str(&" ".repeat(start_col));
            carets.push_str(&"^".repeat(ltxt.len() - start_col).bright_red().bold().to_string());
        } else if l_id == end_line {
            carets.push_str(&"^".repeat(end_col).bright_red().bold().to_string());
        } else {
            carets.push_str(&"^".repeat(ltxt.len()).bright_red().bold().to_string());
        }
        output.push_str(&format!("{:w$} {} {}\n", " ", "│".cyan().bold(), carets, w = digits_len as usize));
    }

    output.push_str(&format!("{:w$} {}\n", " ", "│".cyan().bold(), w = digits_len as usize));
}

impl Diagnostic {
    pub fn display(&self, line_starts: &[usize], lines: &[&str]) -> String {
        use colored::Colorize;

        let mut output = String::new();

        let start_line = determine_line(&line_starts, self.primary_span.start);
        let start_col = self.primary_span.start - line_starts[start_line];
        
        let end_line = determine_line(&line_starts, self.primary_span.end);
        let end_col = self.primary_span.end - line_starts[end_line];

        let digits_len = (end_line + 1).ilog10() + 1;

        output.push_str(&format!("{}: {}\n", "error".bright_red().bold(), self.primary_err));
        create_src_ref(&mut output, digits_len, &self.path, start_line, start_col, end_line, end_col, lines);

        for (msg, span) in &self.secondary_messages {
            let start_line = determine_line(&line_starts, span.start);
            let start_col = span.start - line_starts[start_line];
            
            let end_line = determine_line(&line_starts, span.start);
            let end_col = span.end - line_starts[end_line];

            let digits_len = (end_line + 1).ilog10() + 1;

            if let Some(m) = msg { output.push_str(&format!("{m}\n")) }
            create_src_ref_without_path(&mut output, digits_len, start_line, start_col, end_line, end_col, lines);
        }

        output
    }
}